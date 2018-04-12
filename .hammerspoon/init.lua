require("utils")

local hotkey = require "hs.hotkey"
local alert = require "hs.alert"
hs.window.animationDuration = 0

hyper = {"shift", "alt", "ctrl"}
--shift_hyper = {"shift", "cmd", "alt", "ctrl"}
super_hyper = {"cmd", "shift", "alt", "ctrl"}

-- -----------------------------------------------------------------------------
-- Setup SpoonInstall so that we can install other spoons.
-- -----------------------------------------------------------------------------
hs.loadSpoon("SpoonInstall")
spoonInstall = spoon.SpoonInstall
spoonInstall.use_syncinstall = true
spoonInstall:updateAllRepos()

-- -----------------------------------------------------------------------------
-- System Management
-- -----------------------------------------------------------------------------
local caffeinate = require "hs.caffeinate"

-- Lockscreen
hotkey.bind(hyper, "q", "Lock", function()
  caffeinate.lockScreen()
end)

-- caffeine functionality. icon images from keepingyouawake.
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

-- hotkey.bind({"cmd","shift"},"c", function()
--       setCaffeineDisplay(caffeinate.toggle("displayIdle"))
-- end)

-- -----------------------------------------------------------------------------
-- Window Management with ChunkWM - emulate i3 but use hyper as modifier.
-- -----------------------------------------------------------------------------
mod1 = hyper -- {"alt"}
mod2 = super_hyper --shift_hyper -- {"alt", "shift"}

-- chunkwm doesn't correctly recognize hotplugging monitors. Current solution
-- is to restart: https://github.com/koekeishiya/chunkwm/issues/313
hotkey.bind(hyper, 'c', function()
      hs.execute("brew services restart chunkwm", true)
end)

bindings = {
   -- Focus window
   { mod = mod1, key = 'h', command = "chunkc tiling::window --focus west" },
   { mod = mod1, key = 'j', command = "chunkc tiling::window --focus south" },
   { mod = mod1, key = 'k', command = "chunkc tiling::window --focus north" },
   { mod = mod1, key = 'l', command = "chunkc tiling::window --focus east" },
   -- Fullscreen
   { mod = mod1, key = 'f', command = "chunkc tiling::window --toggle fullscreen" },
   -- Moving windows (swapping, actually)
   { mod = mod2, key = 'h', command = "chunkc tiling::window --swap west" },
   { mod = mod2, key = 'j', command = "chunkc tiling::window --swap south" },
   { mod = mod2, key = 'k', command = "chunkc tiling::window --swap north" },
   { mod = mod2, key = 'l', command = "chunkc tiling::window --swap east" },
   -- Resize window - grow & shrink
   -- { mod = mod1, key = 'left', command = "chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge west" },
   -- { mod = mod1, key = 'down', command = "chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge south" },
   -- { mod = mod1, key = 'up', command = "chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge north" },
   -- { mod = mod1, key = 'right', command = "chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge east" },
   -- { mod = mod2, key = 'right', command = "chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge west" },
   -- { mod = mod2, key = 'up', command = "chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge south" },
   -- { mod = mod2, key = 'down', command = "chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge north" },
   -- { mod = mod2, key = 'left', command = "chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge east" },
   -- Reset and force windows to their original size
   { mod = mod1, key = '=', command = "chunkc tiling::desktop --equalize"},
   -- Close window
   { mod = mod2, key = 'q', command = "chunkc tiling::window --close" },
}

for _, binding in ipairs(bindings) do
   hotkey.bind(binding.mod, binding.key, function()
                  hs.execute(binding.command, true)
   end)
end

resize_mode = hs.hotkey.modal.new(hyper, "r")

alert_uuid = nil

function resize_mode:entered()
   alert_uuid = hs.alert.show("Window Resize Mode", true)
end

function resize_mode:exited()
   hs.alert.closeSpecific(alert_uuid)
end

resize_mode:bind({}, 'escape', function() resize_mode:exit() end)
resize_mode:bind({}, 'return', function() resize_mode:exit() end)
resize_mode:bind(hyper, 'r', function() resize_mode:exit() end)
-- left will shrink the window’s width.
resize_mode:bind({}, 'left', function()
      hs.execute("chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge east", true)
end)
-- right will grow the window’s width.
resize_mode:bind({}, 'right', function()
      hs.execute("chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge east", true)
end)
-- up will shrink the window’s height.
resize_mode:bind({}, 'up', function()
      hs.execute("chunkc tiling::window --use-temporary-ratio -0.1 --adjust-window-edge south", true)
end)
-- down will grow the window’s height.
resize_mode:bind({}, 'down', function()
      hs.execute("chunkc tiling::window --use-temporary-ratio 0.1 --adjust-window-edge south", true)
end)

-- -----------------------------------------------------------------------------
-- Window Management - not useful since I'm using a tiling WM.
-- -----------------------------------------------------------------------------
-- spoonInstall:installSpoonFromRepo("MiroWindowsManager")
-- hs.loadSpoon("MiroWindowsManager")
-- spoon.MiroWindowsManager:bindHotkeys({
--   up = {hyper, "up"},
--   right = {hyper, "right"},
--   down = {hyper, "down"},
--   left = {hyper, "left"},
--   fullscreen = {hyper, "f"}
-- })

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
   {key = '4', name = 'Alacritty'},
   {key = '5', name = 'iTerm'},
   {key = '6', name = 'Finder'},
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
-- Monitor battery power source and rebind keys if necessary.
-- -----------------------------------------------------------------------------
local battery = require "hs.battery"
local currentPowerSource = ""
local browser = ""
--local appsOnACPowerOnly = {"Backup and Sync from Google", "Dropbox"}

function watchBatteryPowerSource()
   local powerSource = battery.powerSource()
   if currentPowerSource ~= powerSource then
      currentPowerSource = powerSource
      local isOnBattery = powerSource == 'Battery Power'

      -- for _, appName in ipairs(appsOnACPowerOnly) do
      --    local app = {hs.application.find(appName)}
      --    if isOnBattery then
      --       if next(app) ~= nil then
      --          for _, id in ipairs(app) do
      --             id:kill(9)
      --          end
      --       end
      --    else
      --       if next(app) == nil then
      --          hs.application.launchOrFocus(appName)
      --       end
      --    end
      -- end

      browser = "Firefox"
      if isOnBattery then
         browser = "Safari"
      end

      hotkey.bind(hyper, '1', browser, function()
                     hs.application.launchOrFocus(browser)
      end)

   end
end

battery.watcher.new(watchBatteryPowerSource):start()
watchBatteryPowerSource()

-- -----------------------------------------------------------------------------
-- Watch for monitor configuration changes, and restart chunkwm everytime
-- the monitor changes.
-- -----------------------------------------------------------------------------
local laptop_screen = "Color LCD"
local home_screen = "FHD2303L"
local work_screen = ""

local timer = null
local DELAY = 5

common_window_layout = {
  {'Safari', nil, laptop_screen, hs.layout.maximized, nil, nil},
  {'Firefox', nil, laptop_screen, hs.layout.maximized, nil, nil},
  {'Mail', nil, laptop_screen, hs.layout.maximized, nil, nil},
}

function enforce_layout ()
   local all_screens = hs.screen.allScreens()
   local screen_count = table_length(all_screens)

   if (screen_count == 2) then
      local second_screen = all_screens[2]
      local window_layout = {
         {"Emacs", nil, second_screen, hs.layout.maximized, nil, nil},
         {"iTerm", nil, second_screen, hs.layout.maximized, nil, nil},
         {"Alacritty", nil, second_screen, hs.layout.maximized, nil, nil},
      }

      local final_layout = {}
      table_merge(final_layout, common_window_layout)
      table_merge(final_layout, window_layout)
      hs.layout.apply(final_layout)
   end
end

function screen_watcher_handler ()
   if (timer) then timer:stop() end
   timer = hs.timer.doAfter(DELAY, function()
                               enforce_layout()
   end)
end

local screen_watcher = hs.screen.watcher.new(screen_watcher_handler)
--screen_watcher:start()

-- -----------------------------------------------------------------------------
-- Show the mouse when its hiding somewhere.
-- -----------------------------------------------------------------------------
spoonInstall:installSpoonFromRepo("MouseCircle")
hs.loadSpoon("MouseCircle")
spoon.MouseCircle:bindHotkeys({
  show = { hyper, "m" }
})

-- -----------------------------------------------------------------------------
-- Reload config automatically upon change.
-- -----------------------------------------------------------------------------
spoonInstall:installSpoonFromRepo("ReloadConfiguration")
hs.loadSpoon("ReloadConfiguration")
--spoon.ReloadConfiguration:start()
-- function reloadConfig(files)
--     doReload = false
--     for _,file in pairs(files) do
--         if file:sub(-4) == ".lua" then
--             doReload = true
--         end
--     end
--     if doReload then
--         hs.reload()
--     end
-- end
-- local myWatcher = hs.pathwatcher.new("/Users/api/.hammerspoon/",
--                                      reloadConfig):start()
alert.show("Hammerspoon config loaded")
