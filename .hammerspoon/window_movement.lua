--------------------------------------------------------------------------------
-- Window Movement
--------------------------------------------------------------------------------

-- TODO: review this
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



-- position by relative screen size
function positionRelativeTo(pos)
   local w = hs.window.focusedWindow()
   local f = w:frame()

   f.x = 
   f.y =
   f.w =
   f.h =
   w:setFrame(f)
end


-- hard positions

function topleft(win)
   win:movetounit(geometry.rect(0, 0, 0.5, 0.5))
end

function top(win)
   win:movetounit(geometry.rect(0, 0, 1, 0.5))
end

function topright(win)
   win:movetounit(geometry.rect(0.5, 0, 0.5, 0.5))
end

function left(win)
   win:movetounit(geometry.rect(0, 0, 0.5, 1))
end

function full(win)
   win:movetounit(geometry.rect(0, 0, 1, 1))
end

function right(win)
   win:movetounit(geometry.rect(0.5, 0, 0.5, 1))
end

function bottomleft(win)
   win:movetounit(geometry.rect(0, 0.5, 0.5, 0.5))
end

function bottom(win)
   win:movetounit(geometry.rect(0, 0.5, 1, 0.5))
end

function bottomright(win)
   win:movetounit(geometry.rect(0.5, 0.5, 0.5, 0.5))
end


function left3(win)
   win:movetounit(geometry.rect(0, 0, 0.33, 1))
end

function left2of3(win)
   win:movetounit(geometry.rect(0, 0, 0.66, 1))
end

function right3(win)
   win:movetounit(geometry.rect(0.66, 0, 0.33, 1))
end

function right2of3(win)
   win:movetounit(geometry.rect(0.33, 0, 0.66, 1))
end

function center3(win)
   win:movetounit(geometry.rect(0.33, 0, 0.33, 1))
end




hotkey.bind({"cmd", "ctrl", "alt"}, "j",
   function()
      left(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "k",
   function()
      full(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "l",
   function()
      right(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "u",
   function()
      top(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "n",
   function()
      bottom(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "i",
   function()
      topleft(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "m",
   function()
      bottomleft(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "o",
   function()
      topright(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, ",",
   function()
      bottomright(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "[",
   function()
      local win = window.focusedwindow()
      wintoscreen(win, win:screen():next())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "y",
   function()
      left3(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "p",
   function()
      left2of3(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "f",
   function()
      center3(window.focusedwindow())
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "g",
   function()
      right3(window.focusedwindow())   
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "c",
   function()
      right2of3(window.focusedwindow())
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

hotkey.bind({"cmd", "ctrl", "alt"}, "left",
   function()
      nudgeLeft(window.focusedwindow(), 10)
      resize(window.focusedwindow(), 15)
   end
)
hotkey.bind({"cmd", "ctrl", "alt"}, "right",
   function()
      resize(window.focusedwindow(), 20)
   end
)



-- Layouts

hotkey.bind({"cmd", "ctrl", "alt"}, "1",
   function()
      local screens = screen.allscreens()
      local apps = {
         ["Google Chrome"]              = {"full", screens[1]},
         ["Safari"]                     = {"full", screens[1]},
         ["Firefox"]                    = {"full", screens[1]},
         ["FirefoxDeveloperEdition"]    = {"full", screens[1]},
         ["Emacs"]                      = {"full", screens[1]},
         ["Xcode"]                      = {"full", screens[1]},
         ["iTerm2"]                     = {"full", screens[1]},
         ["Slack"]                      = {"full", screens[1]},
         ["Spotify"]                    = {"full", screens[1]},
         ["KeePassX"]                   = {"full", screens[1]}
      }

      for name, pos in pairs(apps) do
         local w = winfromtitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "2",
   function()
      local screens = screen.allscreens()
      local apps = {
         ["Google Chrome"]              = {"right", screens[1]},
         ["Safari"]                     = {"right", screens[1]},
         ["Firefox"]                    = {"right", screens[1]},
         ["FirefoxDeveloperEdition"]    = {"right", screens[1]},
         ["Emacs"]                      = {"left", screens[1]},
         ["Xcode"]                      = {"left", screens[1]},
         ["iTerm2"]                     = {"right", screens[1]},
         ["Slack"]                      = {"right", screens[1]},
         ["Spotify"]                    = {"right", screens[1]},
         ["KeePassX"]                   = {"left", screens[1]}
      }

      for name, pos in pairs(apps) do
         local w = winfromtitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)

hotkey.bind({"cmd", "ctrl", "alt"}, "3",
   function()
      local screens = screen.allscreens()
      local apps = {
         ["Google Chrome"]              = {"right3", screens[1]},
         ["Safari"]                     = {"right3", screens[1]},
         ["Firefox"]                    = {"right3", screens[1]},
         ["FirefoxDeveloperEdition"]    = {"right3", screens[1]},
         ["Emacs"]                      = {"center3", screens[1]},
         ["Xcode"]                      = {"left3", screens[1]},
         ["iTerm2"]                     = {"left3", screens[1]},
         ["Slack"]                      = {"left3", screens[1]},
         ["Spotify"]                    = {"center3", screens[1]},
         ["KeePassX"]                   = {"left3", screens[1]}
      }

      for name, pos in pairs(apps) do
         local w = winfromtitle(name)
         if w then
            move(w, pos[1], pos[2])
         end
      end
   end
)