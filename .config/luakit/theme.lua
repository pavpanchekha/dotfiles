--------------------------
-- Default luakit theme --
--------------------------

local theme = {}

-- Default settings
theme.font = "terminus normal 15"

-- Tab label
theme.tab_fg            = "#fff"
theme.tab_bg            = "#666"
theme.tab_ntheme        = "#fff"
theme.selected_fg       = "#fff"
theme.selected_bg       = "#000"
theme.selected_ntheme   = "#fff"
theme.loading_fg        = "#33AADD"
theme.loading_bg        = "#000"

-- Trusted/untrusted ssl colours
theme.trust_fg          = "#0F0"
theme.notrust_fg        = "#F00"

return theme
-- vim: et:sw=4:ts=8:sts=4:tw=80
