function theme --description 'Change desktop theme'
    switch $argv[1]
        case light
            plasma-apply-lookandfeel -a org.kde.breeze.desktop
            alacritty-theme-swap light
        case dark
            plasma-apply-lookandfeel -a org.kde.breezedark.desktop
            alacritty-theme-swap dark
        case '*'
            echo "Usage: theme <light|dark>"
    end
end
