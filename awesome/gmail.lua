---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
---------------------------------------------------

-- {{{ Grab environment
local type = type
local tonumber = tonumber
local io = { popen = io.popen }
local setmetatable = setmetatable
local util = require("awful.util")
local string = {
    find = string.find,
    match = string.match,
    gsub = string.gsub
}
-- }}}


-- Gmail: provides count of new and subject of last e-mail on Gmail
module("vicious.widgets.gmail")


-- {{{ Variable definitions
local rss = {
    inbox   = {
        "https://mail.google.com/mail/feed/atom",
        "Gmail %- Inbox"
    },
    unread  = {
        "https://mail.google.com/mail/feed/atom/unread",
        "Gmail %- Label"
    },
    --labelname = {
    --  "https://mail.google.com/mail/feed/atom/labelname",
    --  "Gmail %- Label"
    --},
}

-- Default is just Inbox
local feed = rss.inbox
local mail = {
    ["{count}"]   = 0,
    ["{subject}"] = "N/A"
}
-- }}}


-- {{{ Gmail widget type
local function worker(format, warg)
    -- Get info from the Gmail atom feed
    local run_once = nil
    local f = io.popen("curl --connect-timeout 1 -m 3 -fsn " .. feed[1])

    for line in f:lines() do
        mail["{count}"] = -- Count comes before messages and matches at least 0
        tonumber(string.match(line, "<fullcount>([%d]+)</fullcount>")) or mail["{count}"]

        -- Find subject tags
        local title = string.match(line, "<title>(.*)</title>")

        if title ~= nil and not string.find(title, feed[2]) then

            -- Spam sanitize the subject and store
            if (run_once) then
                mail["{subject}"] = mail["{subject}"] .. "\n <b>*</b> " .. util.unescape(title) .. " "
            else
                mail["{subject}"] = "<b>*</b> " .. util.unescape(title) .. " "
                run_once = true
            end
        end
    end

    f:close()
    return mail
end
-- }}}

setmetatable(_M, { __call = function(_, ...) return worker(...) end })
