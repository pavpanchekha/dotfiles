
def gmail_to_local(folder):
    if folder.startswith("[Gmail]"):
        name = folder[len("[Gmail]/"):].lower()
        newname = {"sent mail": "sent", "starred": "flagged"}.get(name, name)
        return newname
    elif folder == "INBOX":
        return "inbox"
    else:
        return folder

def local_to_gmail(folder):
    if folder in "chats drafts important sent flagged".split():
        newname = "[Gmail]." + {"sent": "sent mail", "flagged": "starred"}.get(folder, folder).title()
        return newname
    elif folder == "inbox":
        return "INBOX"
    else:
        return folder
