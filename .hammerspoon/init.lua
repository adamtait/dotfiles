

function getDirectoryPath(filePath)
   return string.gsub(filePath, "/%w-$", "")
end


lastRunTime = 0

--~ run bash "touch" on all files at path
function touchAllFilesAt(files)
   local offset = os.time() - lastRunTime
   if offset > 100 then
      for _,filePath in pairs(files) do
          local dirPath = getDirectoryPath(filePath)
          os.execute("touch " .. dirPath .. "/*")
          print('retouched ' .. dirPath)
      end
      lastRunTime = os.time()
   end    
end


local filePath = os.getenv("HOME") .. "/.hammerspoon/paths"

for path in io.lines(filePath) do
   local w = hs.pathwatcher.new(path, touchAllFilesAt):start()
   print("set watcher on " .. path)
end