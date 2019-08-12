#Death to Fish's greeting message!
set fish_greeting
echo ""; #Instead we move down a line


#If we are not in a tty disable the X11 bell
if test "$DISPLAY" != ""
	xset -b
end


set -x fish_term24bit 1 #FULL COLOR!
set -Ux EDITOR micro #I'm lazy :p
set -Ux STEAM_RUNTIME 1


#Globally access Rust and Zig binaries
set -gx PATH $PATH $HOME/.cargo/bin $HOME/Stuff/zig/bin



#For ease of SkyOfSteel development
alias sos="cd ~/Stuff/SkyOfSteel"
alias godot-mono="/home/forloveofcats/Downloads/Godot_v3.1.1-stable_mono_x11_64/Godot_v3.1.1-stable_mono_x11.64"


#Make C-c play nice with my multi-line custom prompt
bind \cc 'echo; echo; commandline -r ""; commandline -f repaint'

#Always terminate the shell when pressing C-d
bind \cd 'exit'
