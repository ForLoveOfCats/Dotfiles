#Death to Fish's greeting message!
set fish_greeting
echo ""; #Instead we move down a line


#If we are not in a tty disable the X11 bell
if test "$DISPLAY" != ""
	xset -b
end


function puke
	if test (count $argv) = 1
		$argv[1] & disown (jobs -lp)
	else
		$argv[1] $argv[2..(count $argv)] & disown (jobs -lp)
	end
end


set -x fish_term24bit 1 #FULL COLOR!
set -Ux EDITOR micro #I'm lazy :p
set -Ux STEAM_RUNTIME 1


#Globally access Rust, Odin, and Nim binaries ect
set -gx PATH $PATH $HOME/.cargo/bin $HOME/Stuff/zig/build $HOME/Stuff/Odin $HOME/.nimble/bin $HOME/.npm/bin $HOME/Stuff/emacs/src $HOME/Stuff/lite



#For ease of SkyOfSteel development
alias sos="cd ~/Stuff/SkyOfSteel"
alias godot-mono="/home/forloveofcats/Downloads/Godot_v3.2-stable_mono_x11_64/Godot_v3.2-stable_mono_x11.64"


#Because xclip doesn't play nice with the X clipboard
alias clip="xclip -selection clipboard"


#Make C-c play nice with my multi-line custom prompt
bind \cc 'echo; echo; commandline -r ""; commandline -f repaint'

#Always terminate the shell when pressing C-d
bind \cd 'exit'
