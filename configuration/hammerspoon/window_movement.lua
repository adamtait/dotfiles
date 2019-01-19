--------------------------------------------------------------------------------
-- Window Movement
--------------------------------------------------------------------------------


-- position by relative screen size
function positionRelativeTo(x, y, w, h)
   local win = hs.window.focusedWindow()
   local f = win:frame()
   local screen = win:screen()
   local max = screen:frame()

   f.x = max.x + (max.w * x)
   f.y = max.y + (max.h * y)
   f.w = max.w * w
   f.h = max.h * h
   win:setFrame(f)
end




-- hard positions

function topleft()
   positionRelativeTo(0, 0, 0.5, 0.5)
end

function top()
   positionRelativeTo(0, 0, 1, 0.5)
end

function topright()
   positionRelativeTo(0.5, 0, 0.5, 0.5)
end

function left()
   positionRelativeTo(0, 0, 0.5, 1)
end

function full()
   positionRelativeTo(0, 0, 1, 1)
end

function right()
   positionRelativeTo(0.5, 0, 0.5, 1)
end

function bottomleft()
   positionRelativeTo(0, 0.5, 0.5, 0.5)
end

function bottom()
   positionRelativeTo(0, 0.5, 1, 0.5)
end

function bottomright()
   positionRelativeTo(0.5, 0.5, 0.5, 0.5)
end


function left3()
   positionRelativeTo(0, 0, 0.33, 1)
end

function left2of3()
   positionRelativeTo(0, 0, 0.66, 1)
end

function right3()
   positionRelativeTo(0.66, 0, 0.34, 1)
end

function right2of3()
   positionRelativeTo(0.33, 0, 0.67, 1)
end

function center3()
   positionRelativeTo(0.33, 0, 0.33, 1)
end




hs.hotkey.bind({"cmd", "ctrl", "alt"}, "j",
   function()
      left()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "k",
   function()
      full()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "l",
   function()
      right()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "u",
   function()
      top()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "n",
   function()
      bottom()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "i",
   function()
      topleft()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "m",
   function()
      bottomleft()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "o",
   function()
      topright()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, ",",
   function()
      bottomright()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "[",
   function()
      local win = window.focusedwindow()
      wintoscreen(win, win:screen():next())
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "y",
   function()
      left3()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "p",
   function()
      left2of3()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "f",
   function()
      center3()
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "g",
   function()
      right3()   
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "c",
   function()
      right2of3()
   end
)



-- Nudging & Resizing

function nudge(win, distance)
   local f = win:frame()
   f.x = f.x + distance
   win:setFrame(f)
end

function resize(win, distance)
   local size = win:size()
   size.w = size.w + distance
   win:setSize(size)
end



hs.hotkey.bind({"cmd", "ctrl", "alt"}, "Left",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      if f.x == 0 then
         resize(win, -20)
      else
         nudge(win, -20)
         resize(win, 20)
      end
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "Right",
   function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      if f.x == 0 then
         resize(win, 20)
      else
         nudge(win, 20)
         resize(win, -20)
      end
   end
)







-- Layouts

function screenName()
  return hs.screen.allScreens()[1]:name()
end
  

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "1",
   function()
      local n = screenName()
      local layout = {
         {"Google Chrome",              nil, n, hs.layout.maximized, nil, nil},
         {"Safari",                     nil, n, hs.layout.maximized, nil, nil},
         {"Firefox",                    nil, n, hs.layout.maximized, nil, nil},
         {"Firefox Developer Edition",  nil, n, hs.layout.maximized, nil, nil},
         {"Emacs",                      nil, n, hs.layout.maximized, nil, nil},
         {"Xcode",                      nil, n, hs.layout.maximized, nil, nil},
         {"iTerm2",                     nil, n, hs.layout.maximized, nil, nil},
         {"Slack",                      nil, n, hs.layout.maximized, nil, nil},
         {"Spotify",                    nil, n, hs.layout.maximized, nil, nil},
         {"KeePassX",                   nil, n, hs.layout.maximized, nil, nil}
      }
      hs.layout.apply(layout)
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "2",
   function()
      local n = screenName()
      local layout = {
         {"Google Chrome",              nil, n, hs.layout.right50, nil, nil},
         {"Safari",                     nil, n, hs.layout.right50, nil, nil},
         {"Firefox",                    nil, n, hs.layout.right50, nil, nil},
         {"Firefox Developer Edition",  nil, n, hs.layout.right50, nil, nil},
         {"Emacs",                      nil, n, hs.layout.left50, nil, nil},
         {"Xcode",                      nil, n, hs.layout.left50, nil, nil},
         {"iTerm2",                     nil, n, hs.layout.right50, nil, nil},
         {"Slack",                      nil, n, hs.layout.right50, nil, nil},
         {"Spotify",                    nil, n, hs.layout.right50, nil, nil},
         {"KeePassX",                   nil, n, hs.layout.left50, nil, nil}
      }
      hs.layout.apply(layout)
   end
)

hs.hotkey.bind({"cmd", "ctrl", "alt"}, "3",
   function()
      local n = screenName()
      local l = hs.geometry.rect(0, 0, 0.33, 1)
      local m = hs.geometry.rect(0.33, 0, 0.33, 1)
      local r = hs.geometry.rect(0.66, 0, 0.33, 1)      

      local layout = {
         {"Google Chrome",              nil, n, r, nil, nil},
         {"Safari",                     nil, n, r, nil, nil},
         {"Firefox",                    nil, n, r, nil, nil},
         {"Firefox Developer Edition",  nil, n, r, nil, nil},
         {"Emacs",                      nil, n, m, nil, nil},
         {"Xcode",                      nil, n, l, nil, nil},
         {"iTerm2",                     nil, n, l, nil, nil},
         {"Slack",                      nil, n, l, nil, nil},
         {"Spotify",                    nil, n, m, nil, nil},
         {"KeePassX",                   nil, n, l, nil, nil}
      }
      hs.layout.apply(layout)
   end
)
