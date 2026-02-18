// Build: swiftc -O /Users/pavpan/watchproc/top-cpu-widget.swift -o "$HOME/bin/top-cpu-widget"
//
// top-cpu-widget
// - Menu bar app that shows the currently running process with the highest
//   accumulated CPU time over a rolling window.
// - Default behavior:
//   - Window: 60 minutes
//   - Sample interval: 1 second
// - Useful for spotting runaway benchmark processes.
//
// Runtime options:
// - CLI flags:
//   - --window-minutes <minutes>
//   - --sample-seconds <seconds>
//   - --print-once
// - Environment variables:
//   - TOP_CPU_WINDOW_MINUTES
//   - TOP_CPU_SAMPLE_SECONDS
//
// Notes:
// - Only currently running processes are considered.
// - CPU time is computed as user+system CPU-time delta inside the rolling window.
// - Accuracy improves as more samples are collected over time.
//
// launchd:
// - Install via /Users/pavpan/watchproc/scripts/install-launch-agent.sh
// - LaunchAgent target binary: /Users/pavpan/bin/top-cpu-widget

import AppKit
import Foundation
import Darwin

struct Config {
    let window: TimeInterval
    let sampleInterval: TimeInterval
    let printOnce: Bool

    static func parse() -> Config {
        let env = ProcessInfo.processInfo.environment
        var windowMinutes = Double(env["TOP_CPU_WINDOW_MINUTES"] ?? "") ?? 60
        var sampleSeconds = Double(env["TOP_CPU_SAMPLE_SECONDS"] ?? "") ?? 1
        var printOnce = false

        var i = 1
        let args = CommandLine.arguments
        while i < args.count {
            switch args[i] {
            case "--window-minutes":
                if i + 1 < args.count, let v = Double(args[i + 1]), v > 0 {
                    windowMinutes = v
                    i += 1
                }
            case "--sample-seconds":
                if i + 1 < args.count, let v = Double(args[i + 1]), v > 0 {
                    sampleSeconds = v
                    i += 1
                }
            case "--print-once":
                printOnce = true
            default:
                break
            }
            i += 1
        }

        return Config(
            window: windowMinutes * 60,
            sampleInterval: sampleSeconds,
            printOnce: printOnce
        )
    }
}

struct Snapshot {
    let timestamp: Date
    let totalCPUTimeNS: UInt64
}

struct TopProcess {
    let pid: pid_t
    let name: String
    let cpuTimeInWindowNS: UInt64
    let cpuPercent: Double
}

final class ProcessTracker {
    private let config: Config
    private var history: [pid_t: [Snapshot]] = [:]
    private var names: [pid_t: String] = [:]

    init(config: Config) {
        self.config = config
    }

    func sampleNow() {
        let now = Date()
        let running = captureRunningProcesses()

        for (pid, cpuNS, name) in running {
            names[pid] = name
            var snapshots = history[pid] ?? []
            if let last = snapshots.last, last.totalCPUTimeNS == cpuNS {
                continue
            }
            snapshots.append(Snapshot(timestamp: now, totalCPUTimeNS: cpuNS))
            history[pid] = snapshots
        }

        let activePIDs = Set(running.map { $0.0 })
        prune(now: now, activePIDs: activePIDs)
    }

    func topProcessNow() -> TopProcess? {
        let now = Date()
        let running = captureRunningProcesses()
        let activePIDs = Set(running.map { $0.0 })
        let windowStart = now.addingTimeInterval(-config.window)

        var best: TopProcess?

        for (pid, cpuNS, name) in running {
            guard var snapshots = history[pid], !snapshots.isEmpty else {
                continue
            }

            if snapshots.last?.totalCPUTimeNS != cpuNS {
                snapshots.append(Snapshot(timestamp: now, totalCPUTimeNS: cpuNS))
                history[pid] = snapshots
            }

            guard let latest = snapshots.last else { continue }

            var baseline = snapshots[0]
            if let index = snapshots.lastIndex(where: { $0.timestamp <= windowStart }) {
                baseline = snapshots[index]
            } else if let firstInWindow = snapshots.first(where: { $0.timestamp >= windowStart }) {
                baseline = firstInWindow
            }

            let delta = latest.totalCPUTimeNS >= baseline.totalCPUTimeNS
                ? latest.totalCPUTimeNS - baseline.totalCPUTimeNS
                : 0
            let elapsed = latest.timestamp.timeIntervalSince(baseline.timestamp)
            let cpuPercent = elapsed > 0
                ? (Double(delta) / 1_000_000_000.0) / elapsed * 100.0
                : 0

            if let currentBest = best {
                if delta > currentBest.cpuTimeInWindowNS {
                    best = TopProcess(
                        pid: pid,
                        name: name,
                        cpuTimeInWindowNS: delta,
                        cpuPercent: cpuPercent
                    )
                }
            } else {
                best = TopProcess(
                    pid: pid,
                    name: name,
                    cpuTimeInWindowNS: delta,
                    cpuPercent: cpuPercent
                )
            }
        }

        prune(now: now, activePIDs: activePIDs)
        return best
    }

    private func prune(now: Date, activePIDs: Set<pid_t>) {
        let keepSince = now.addingTimeInterval(-config.window - max(2 * config.sampleInterval, 30))
        var newHistory: [pid_t: [Snapshot]] = [:]
        var newNames: [pid_t: String] = [:]

        for pid in activePIDs {
            guard var snapshots = history[pid], !snapshots.isEmpty else { continue }

            snapshots.removeAll(where: { $0.timestamp < keepSince })
            if snapshots.isEmpty {
                continue
            }

            newHistory[pid] = snapshots
            newNames[pid] = names[pid] ?? "pid \(pid)"
        }

        history = newHistory
        names = newNames
    }

    private func captureRunningProcesses() -> [(pid_t, UInt64, String)] {
        let pids = listPIDs()
        var out: [(pid_t, UInt64, String)] = []
        out.reserveCapacity(pids.count)

        for pid in pids where pid > 0 {
            guard let cpuNS = cpuTimeNanoseconds(for: pid) else { continue }
            let name = processName(for: pid) ?? "pid \(pid)"
            out.append((pid, cpuNS, name))
        }

        return out
    }

    private func listPIDs() -> [pid_t] {
        let byteCount = proc_listpids(UInt32(PROC_ALL_PIDS), 0, nil, 0)
        guard byteCount > 0 else { return [] }

        let count = Int(byteCount) / MemoryLayout<pid_t>.size
        var pids = Array(repeating: pid_t(0), count: count)
        let filled = proc_listpids(UInt32(PROC_ALL_PIDS), 0, &pids, byteCount)
        guard filled > 0 else { return [] }

        let filledCount = Int(filled) / MemoryLayout<pid_t>.size
        return Array(pids.prefix(filledCount))
    }

    private func cpuTimeNanoseconds(for pid: pid_t) -> UInt64? {
        var taskInfo = proc_taskinfo()
        let result = proc_pidinfo(
            pid,
            PROC_PIDTASKINFO,
            0,
            &taskInfo,
            Int32(MemoryLayout<proc_taskinfo>.size)
        )
        guard result == Int32(MemoryLayout<proc_taskinfo>.size) else {
            return nil
        }

        return taskInfo.pti_total_user + taskInfo.pti_total_system
    }

    private func processName(for pid: pid_t) -> String? {
        if let appName = appBundleName(for: pid) {
            return appName
        }

        if let runningApp = NSRunningApplication(processIdentifier: pid),
           let localizedName = runningApp.localizedName,
           !localizedName.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
            return localizedName
        }

        var buffer = Array(repeating: CChar(0), count: Int(MAXCOMLEN + 1))
        let len = proc_name(pid, &buffer, UInt32(buffer.count))
        if len > 0 {
            let raw = String(cString: buffer).trimmingCharacters(in: .whitespacesAndNewlines)
            if !raw.isEmpty {
                return raw
            }
        }

        if let path = processPath(for: pid) {
            let base = URL(fileURLWithPath: path).lastPathComponent.trimmingCharacters(in: .whitespacesAndNewlines)
            if !base.isEmpty {
                return base
            }
        }

        return nil
    }

    private func appBundleName(for pid: pid_t) -> String? {
        guard let fullPath = processPath(for: pid) else { return nil }

        guard let appRange = fullPath.range(of: ".app/Contents/") else { return nil }

        let appPath = String(fullPath[..<appRange.upperBound]).replacingOccurrences(of: "/Contents/", with: "")
        let appURL = URL(fileURLWithPath: appPath)
        let appName = appURL.deletingPathExtension().lastPathComponent
        return appName.isEmpty ? nil : appName
    }

    private func processPath(for pid: pid_t) -> String? {
        var pathBuffer = Array(repeating: CChar(0), count: 4096)
        let len = proc_pidpath(pid, &pathBuffer, UInt32(pathBuffer.count))
        guard len > 0 else { return nil }
        return String(cString: pathBuffer)
    }
}

func formatPercent(_ value: Double) -> String {
    return String(format: "%.1f", value)
}

final class AppDelegate: NSObject, NSApplicationDelegate {
    let config = Config.parse()
    private lazy var tracker = ProcessTracker(config: config)

    private var statusItem: NSStatusItem?
    private var timer: Timer?
    private var currentTop: TopProcess?
    private var killMenuItem: NSMenuItem?
    private var forceKillMenuItem: NSMenuItem?

    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApplication.shared.setActivationPolicy(.accessory)

        let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
        item.button?.title = "starting..."
        statusItem = item

        let menu = NSMenu()
        let refreshItem = NSMenuItem(title: "Refresh now", action: #selector(refresh), keyEquivalent: "r")
        refreshItem.target = self
        menu.addItem(refreshItem)

        let killItem = NSMenuItem(title: "Kill", action: #selector(killTopProcess), keyEquivalent: "k")
        killItem.target = self
        menu.addItem(killItem)
        killMenuItem = killItem

        let forceKillItem = NSMenuItem(title: "Force Kill (-9)", action: #selector(forceKillTopProcess), keyEquivalent: "")
        forceKillItem.target = self
        menu.addItem(forceKillItem)
        forceKillMenuItem = forceKillItem

        menu.addItem(NSMenuItem.separator())
        let quitItem = NSMenuItem(title: "Quit", action: #selector(quit), keyEquivalent: "q")
        quitItem.target = self
        menu.addItem(quitItem)
        item.menu = menu

        tracker.sampleNow()
        updateTitle()

        timer = Timer.scheduledTimer(withTimeInterval: config.sampleInterval, repeats: true) { [weak self] _ in
            self?.refresh()
        }
    }

    @objc private func refresh() {
        tracker.sampleNow()
        updateTitle()
    }

    @objc private func quit() {
        NSApplication.shared.terminate(nil)
    }

    @objc private func killTopProcess() {
        killCurrentTop(signal: SIGTERM)
    }

    @objc private func forceKillTopProcess() {
        killCurrentTop(signal: SIGKILL)
    }

    private func updateTitle() {
        guard let button = statusItem?.button else { return }

        if let top = tracker.topProcessNow() {
            currentTop = top
            let displayName = top.name.trimmingCharacters(in: .whitespacesAndNewlines)
            button.title = "\(displayName) (pid \(top.pid), \(formatPercent(top.cpuPercent))%)"
        } else {
            currentTop = nil
            button.title = "n/a"
        }

        updateKillMenuItems()
    }

    private func updateKillMenuItems() {
        if let top = currentTop {
            let displayName = top.name.trimmingCharacters(in: .whitespacesAndNewlines)
            killMenuItem?.isEnabled = true
            forceKillMenuItem?.isEnabled = true
            killMenuItem?.title = "Kill \(displayName) (pid \(top.pid))"
            forceKillMenuItem?.title = "Force Kill \(displayName) (pid \(top.pid), -9)"
        } else {
            killMenuItem?.isEnabled = false
            forceKillMenuItem?.isEnabled = false
            killMenuItem?.title = "Kill"
            forceKillMenuItem?.title = "Force Kill (-9)"
        }
    }

    private func killCurrentTop(signal: Int32) {
        guard let top = currentTop else {
            return
        }

        if top.pid == getpid() {
            showFailureAlert("Refusing to kill this watcher process.")
            return
        }

        if Darwin.kill(top.pid, signal) != 0 {
            let reason = String(cString: strerror(errno))
            let signalLabel = signal == SIGKILL ? "SIGKILL (-9)" : "SIGTERM"
            showFailureAlert("Failed to send \(signalLabel) to pid \(top.pid): \(reason)")
        } else {
            refresh()
        }
    }

    private func showFailureAlert(_ message: String) {
        NSApp.activate(ignoringOtherApps: true)
        let alert = NSAlert()
        alert.alertStyle = .warning
        alert.messageText = "Kill failed"
        alert.informativeText = message
        alert.addButton(withTitle: "OK")
        alert.runModal()
    }
}

let app = NSApplication.shared
let delegate = AppDelegate()
if delegate.config.printOnce {
    let tracker = ProcessTracker(config: delegate.config)
    tracker.sampleNow()
    if let top = tracker.topProcessNow() {
        print("\(top.name) (pid \(top.pid), \(formatPercent(top.cpuPercent))%)")
    } else {
        print("n/a")
    }
    exit(0)
} else {
    app.delegate = delegate
    app.run()
}
