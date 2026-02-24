function ask
    claude --model haiku -p "$argv" | glow
end
