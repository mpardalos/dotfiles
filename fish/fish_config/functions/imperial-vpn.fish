function imperial-vpn --description 'Manage Imperial College VPN' --argument command
    set -l profile "/home/mpardalos/.config/profile-userlocked-mp5617@ic.ac.uk.ovpn"

    switch $command
        case "up"
            openvpn3 session-start --config $profile
        case "down"
            openvpn3 session-manage --disconnect --config $profile
            openvpn3 session-manage --cleanup
        case '*'
            echo "Usage: imperial-vpn [up|down]"
            return 1
    end
end
