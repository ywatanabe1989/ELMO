# Why
Logo for the project on GitHub, LLEMACS — Neural Information Network of Joint Agents

# Who
5-10 LLEMACS agents working as a team

# How
Purplish
Scientific
Artistic

# What
Keyboard typing in front of monitors
Computer programming
Computer server room
Emacs
HHKB
Harman Mirror Chair

# When
Night

# Where
Japan
Mt. Fuji
Shizuoka
Tokyo
Cyber city


https://magicstudio.com/ai-art-generator/
mv ~/win/downloads/magic*studio-art*.jpg ./docs/logos

for i in ./docs/logos/magicstudio-art*; do 
  num=$(echo $i | grep -o '[0-9]\+' | head -1)
  if [ ! -z "$num" ]; then
    new_name="./docs/logos/logo_$(printf "%02d" $num).jpg"
    if [ ! -f "$new_name" ]; then
      mv "$i" "$new_name"
    fi
  fi
done

ls ./docs/logos
