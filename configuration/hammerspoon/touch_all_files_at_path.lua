-------------------------------
-- Touch All Files at Path
-------------------------------
------ watches a directory for any files that changes. As soon as the change happens, then script touches all files in the directory.
------ PRE: set a ~/.hammerspoon/paths (plaintext) with a list of watched paths

function getDirectoryPath(filePath)
   return string.gsub(filePath, "/[%.%w]-$", "")
end

function bashSafeString(s)
   return string.gsub(s, "([ %(%)])", "\\%1")
end
   

lastRunTime = 0

--~ run bash "touch" on all files at path
function touchAllFilesAt(files)
   local offset = os.time() - lastRunTime
   if offset > 100 then
      for _,filePath in pairs(files) do
          local dirPath = getDirectoryPath(filePath)
          os.execute("touch " .. bashSafeString(dirPath) .. "/*")
          print('retouched ' .. dirPath)
      end
      lastRunTime = os.time()
   end    
end


local filePath = os.getenv("HOME") .. "/.hammerspoon/paths"

for path in io.lines(filePath) do
   hs.pathwatcher.new(path, touchAllFilesAt):start()
   print("set watcher on " .. path)
end