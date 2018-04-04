local hotkey = require "hs.hotkey"
local alert = require "hs.alert"
hs.window.animationDuration = 0

hyper = {"cmd", "alt", "ctrl", "shift"}

-- -----------------------------------------------------------------------------
-- System Management
-- -----------------------------------------------------------------------------
local caffeinate = require "hs.caffeinate"

--
-- Lockscreen
--
hotkey.bind(hyper, "L", "Lock", function()
  caffeinate.lockscreen()
end)

--
-- caffeine functionality. icon images from keepingyouawake.
--
local caffeine = hs.menubar.new()
local activeMessage = "Sleeping prohitited"
local inactiveMessage = "Sleeping allowed"
function setCaffeineDisplay(state)
  if state then
    caffeine:setIcon("caffeine-active.png")
    caffeine:setTooltip(activeMessage)
    alert.show(activeMessage)
  else
    caffeine:setIcon("caffeine-inactive.png")
    caffeine:setTooltip(inactiveMessage)
    alert.show(inactiveMessage)
  end
end

function caffeineClicked()
  setCaffeineDisplay(caffeinate.toggle("displayIdle"))
end

if caffeine then
  caffeine:setClickCallback(caffeineClicked)
  setCaffeineDisplay(caffeinate.get("displayIdle"))
end

hotkey.bind({"cmd","shift"},"c", function()
      setCaffeineDisplay(caffeinate.toggle("displayIdle"))
end)

-- -----------------------------------------------------------------------------
-- Window Management 
-- -----------------------------------------------------------------------------
hs.loadSpoon("MiroWindowsManager")
spoon.MiroWindowsManager:bindHotkeys({
  up = {hyper, "k"},
  right = {hyper, "l"},
  down = {hyper, "j"},
  left = {hyper, "h"},
  fullscreen = {hyper, "F"}
})

-- -----------------------------------------------------------------------------
-- Window Management with ChunkWM
-- -----------------------------------------------------------------------------
keysWindowFunctions = {
    {'0', "chunkc tiling::desktop --equalize"}, -- equalize size of windows.
    {'f', "chunkc tiling::window --toggle fullscreen"},
    {'s', "chunkc tiling::window --swap prev"}
}

for i,kv in ipairs(keysWindowFunctions) do
   hs.hotkey.bind(hyper, kv[1], function() hs.execute(kv[2], true); end) 
end

-- -----------------------------------------------------------------------------
-- Window switching.
-- -----------------------------------------------------------------------------
--
-- cmd-tab replacement using alt-tab. Not useful in most cases.
--
switcher = hs.window.switcher.new(hs.window.filter.new():setDefaultFilter{})
switcher.ui.highlightColor = {0.4,0.4,0.5,0.8}
switcher.ui.thumbnailSize = 128
switcher.ui.selectedThumbnailSize = 284
switcher.ui.backgroundColor = {0.3, 0.3, 0.3, 0.5}
switcher.ui.fontName = 'System'
switcher.ui.showSelectedTitle = false
hs.hotkey.bind("alt", "tab", function()switcher:next()end)
hs.hotkey.bind("alt-shift", "tab", function()switcher:previous()end)

--
-- ace-window style focused-window switcher in a given desktop with hyper-tab.
--
hs.hints.hintChars = {'a','s','d','f','g','h','j','k','l'}
hotkey.bind(hyper, 'tab', function()
                hs.hints.windowHints()
                end)

-- -----------------------------------------------------------------------------
-- Application Management
-- -----------------------------------------------------------------------------
local application = require "hs.application"

--
-- Quick launcher for most used apps bound to hyper + number.
--
keysQuickApps = {
   {key = '1', name = 'Firefox'},
   {key = '2', name = 'Mail'},
   {key = '3', name = 'Emacs'},
   {key = '4', name = 'iTerm'},
}

for _, app in ipairs(keysQuickApps) do
   hotkey.bind(hyper, app.key, app.name, function()
                  hs.application.launchOrFocus(app.name)
   end)
end

--
-- Modal bindings for other frequently used apps bound to [hyper + a] + key.
--
myModal = hotkey.modal.new({}, "F16")

keysApps = {
    {key = 'a', name = 'Airmail 3'},
    {key = 'b', name = 'Firefox'},
    {key = 'e', name = 'Emacs'},
    {key = 'f', name = 'Finder'},
    {key = 'i', name = 'iTerm'},
    {key = 'p', name = 'Skim'},
    {key = 's', name = 'Safari'},
    {key = 't', name = 'Terminal'},
}

for _, app in ipairs(keysApps) do
   if app.id then
      local located_name = hs.application.nameForBundleID(app.id)
      if located_name then
         myModal:bind('', app.key, located_name, function()
                         hs.application.launchOrFocusByBundleID(app.id)
                         myModal:exit()
         end)
      end
   elseif app.name then
      myModal:bind('', app.key, app.name, function()
                      hs.application.launchOrFocus(app.name)
                      myModal:exit()
      end)
   end
end

pressedModal = function() myModal:enter() end
releasedModal = function() end
hotkey.bind(hyper, 'a', nil, pressedModal, releasedModal)

-- -----------------------------------------------------------------------------
-- Reload config automatically upon change.
-- -----------------------------------------------------------------------------
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
local myWatcher = hs.pathwatcher.new("/Users/api/dotfiles/hammerspoon/",
                                     reloadConfig):start()
alert.show("Hammerspoon config loaded")
