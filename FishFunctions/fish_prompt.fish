function fish_prompt
    set_color ffff00
    printf $USER
    set_color normal
    printf ":@"
    set_color 00ff00
    dirs | tr -d '\n'
    printf "\n\$>>> "
end
