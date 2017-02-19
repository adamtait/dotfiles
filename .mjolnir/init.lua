mjolnir.application = require "mjolnir.application"
mjolnir.window      = require "mjolnir.window"
mjolnir.hotkey      = require "mjolnir.hotkey"
mjolnir.fnutils     = require "mjolnir.fnutils"
mjolnir.geometry    = require "mjolnir.geometry"
mjolnir.screen      = require "mjolnir.screen"


-- a couple of these functions were taken from https://github.com/ledbettj/config-files/blob/06bf36b3dec7be4536ccf8427e903ebbcc1de3e1/mjolnir.lua

-- find the main window belonging to the application with title 'title'
function winfromtitle(title)
   local apps = mjolnir.application.runningapplications()
   for _, app in pairs(apps) do
      if app:title() == title then
         return app:mainwindow()
      end
   end

   return nil
end

-- move the given window to the given screen, keeping the same relative
-- dimensions and placement.
function wintoscreen(win, screen)
   if not win or not screen then
      return false
   end

   local old_screen       = win:screen()
   local old_screen_frame = old_screen:frame()
   local win_frame        = win:frame()
   local new_screen       = screen
   local new_frame        = new_screen:frame()
   local off_x_pct        = (win_frame.x - old_screen_frame.x) / (old_screen_frame.w)
   local off_y_pct        = (win_frame.y - old_screen_frame.y) / (old_screen_frame.h)

   local w_pct = win_frame.w / old_screen_frame.w
   local h_pct = win_frame.h / old_screen_frame.h

   win_frame.x = new_frame.x + (off_x_pct * new_frame.w)
   win_frame.y = new_frame.y + (off_y_pct * new_frame.h)
   win_frame.w = w_pct * new_frame.w
   win_frame.h = h_pct * new_frame.h

   win:setframe(win_frame)
end

-- move the given window to the position ('topleft', 'topright', etc),
-- also moving it to the provided screen if any.
function move(win, where, screen)
   if not win or not where then
      return false
   end
   wintoscreen(win, screen)
   _G[where](win)
end



-- hard positions

function topleft(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 0.5, 0.5))
end

function top(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 1, 0.5))
end

function topright(win)
   win:movetounit(mjolnir.geometry.rect(0.5, 0, 0.5, 0.5))
end

function left(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 0.5, 1))
end

function full(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 1, 1))
end

function right(win)
   win:movetounit(mjolnir.geometry.rect(0.5, 0, 0.5, 1))
end

function right3(win)
   win:movetounit(mjolnir.geometry.rect(0.33, 0, 0.33, 1))
end

function bottomleft(win)
   win:movetounit(mjolnir.geometry.rect(0, 0.5, 0.5, 0.5))
end

function bottom(win)
   win:movetounit(mjolnir.geometry.rect(0, 0.5, 1, 0.5))
end

function bottomright(win)
   win:movetounit(mjolnir.geometry.rect(0.5, 0.5, 0.5, 0.5))
end


function left3(win)
   win:movetounit(mjolnir.geometry.rect(0, 0, 0.33, 1))
end

function right3(win)
   win:movetounit(mjolnir.geometry.rect(0.66, 0, 0.33, 1))
end

function center3(win)
   win:movetounit(mjolnir.geometry.rect(0.33, 0, 0.33, 1))
end




mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "j",
   function()
      left(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "k",
   function()
      full(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "l",
   function()
      right(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "u",
   function()
      top(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "n",
   function()
      bottom(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "i",
   function()
      topleft(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "m",
   function()
      bottomleft(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "o",
   function()
      topright(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, ",",
   function()
      bottomright(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "[",
   function()
      local win = mjolnir.window.focusedwindow()
      wintoscreen(win, win:screen():next())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "y",
   function()
      left3(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "f",
   function()
      center3(mjolnir.window.focusedwindow())
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "g",
   function()
      right3(mjolnir.window.focusedwindow())   
   end
)




-- Nudging & Resizing

function nudgeLeft(win, distance)
   local frame = win:frame()
   frame.x = frame.x - distance
   win:setframe(frame)
end

function nudgeRight(win, distance)
   local frame = win:frame()
   frame.x = frame.x + distance
   win:setframe(frame)
end

function resize(win, distance)
   local size = win:size()
   size.w = size.w + distance
   win:setsize(size)
end

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "left",
   function()
      nudgeLeft(mjolnir.window.focusedwindow(), 10)
      resize(mjolnir.window.focusedwindow(), 15)
   end
)
mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "right",
   function()
      resize(mjolnir.window.focusedwindow(), 20)
   end
)



-- Layouts

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "1",
   function()
      local screens = mjolnir.screen.allscreens()
      local apps = {
         ["Google Chrome"]       = {"full", screens[1]},
         ["Safari"]              = {"full", screens[1]},
         ["Emacs"]               = {"full", screens[1]},
         ["Xcode"]               = {"full", screens[1]},
         ["iTerm2"]              = {"full", screens[1]},
         ["Slack"]               = {"full", screens[1]},
         ["Spotify"]             = {"full", screens[1]},
         ["Microsoft Outlook"]   = {"full", screens[1]}
      }

      for name, pos in pairs(apps) do
         local w = winfromtitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "2",
   function()
      local screens = mjolnir.screen.allscreens()
      local apps = {
         ["Google Chrome"]       = {"right", screens[1]},
         ["Safari"]              = {"right", screens[1]},
         ["Emacs"]               = {"left", screens[1]},
         ["Xcode"]               = {"left", screens[1]},
         ["iTerm2"]              = {"right", screens[1]},
         ["Slack"]               = {"right", screens[1]},
         ["Spotify"]             = {"right", screens[1]},
         ["Microsoft Outlook"]   = {"right", screens[1]}
      }

      for name, pos in pairs(apps) do
         local w = winfromtitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)

mjolnir.hotkey.bind({"cmd", "ctrl", "alt"}, "3",
   function()
      local screens = mjolnir.screen.allscreens()
      local apps = {
         ["Google Chrome"]       = {"right3", screens[1]},
         ["Safari"]              = {"right3", screens[1]},
         ["Firefox"]             = {"right3", screens[1]},
         ["Emacs"]               = {"center3", screens[1]},
         ["Xcode"]               = {"left3", screens[1]},
         ["iTerm2"]              = {"left3", screens[1]},
         ["Slack"]               = {"left3", screens[1]},
         ["Spotify"]             = {"center3", screens[1]},
         ["KeePassX"]            = {"left3", screens[1]},
      }

      for name, pos in pairs(apps) do
         local w = winfromtitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)