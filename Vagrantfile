# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box = "archlinux/archlinux"

  config.ssh.forward_x11 = true
  config.ssh.forward_agent = true

  config.vm.provision "shell", inline: <<-SHELL
    pacman -Sy --noconfirm git

    sudo -u vagrant mkdir -p /home/vagrant/.config
    cd /home/vagrant/.config
    sudo -u vagrant git clone https://gitlab.com/michalis_pardalos/dotfiles.git dotfiles

    cd dotfiles
    ./install deps
    sudo -u vagrant ./install

    chsh -s /usr/bin/fish vagrant
  SHELL
end
