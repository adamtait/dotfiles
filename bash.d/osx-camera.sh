#!/bin/sh

alias fix-camera='sudo launchctl stop com.apple.cmio.VDCAssistant && sudo launchctl start com.apple.cmio.VDCAssistant && sudo launchctl stop com.apple.cmio.AppleCameraAssistant && sudo launchctl start com.apple.cmio.AppleCameraAssistant'
