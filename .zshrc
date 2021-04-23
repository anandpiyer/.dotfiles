#------------------------------------------------------------------------------
# 1. Tmux
#------------------------------------------------------------------------------
if [ -z "$TMUX" ]
then
    tmux attach -t TMUX || tmux new -s TMUX
fi

#------------------------------------------------------------------------------
# 2. Exports
#------------------------------------------------------------------------------
#export PATH="/usr/local/bin:/usr/local/sbin:$PATH"
#export PATH="$HOME/.cargo/bin:$PATH"
export EDITOR="emacsclient -c -n"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

#------------------------------------------------------------------------------
# 3. Aliases
#------------------------------------------------------------------------------
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias dotfiles='/usr/local/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias ll="exa -lGx"
alias emacsc='emacsclient -c -n'

#------------------------------------------------------------------------------
# 4. Colors
#------------------------------------------------------------------------------
autoload -U colors && colors
export CLICOLOR="yes"

#------------------------------------------------------------------------------
# 5. Completion
#------------------------------------------------------------------------------
autoload -U compinit && compinit
zmodload -i zsh/complist
setopt auto_menu
setopt complete_in_word

zstyle ':completion:*' list-colors ''

#------------------------------------------------------------------------------
# 6. History
#------------------------------------------------------------------------------
if [ -z $HISTFILE ]; then
    HISTFILE=$HOME/.zsh_history
fi
HISTSIZE=10000
SAVEHIST=10000

setopt append_history
setopt inc_append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt share_history

#------------------------------------------------------------------------------
# 7. Prompt
#------------------------------------------------------------------------------
#autoload -U promptinit && promptinit
#prompt pure

## http://zanshin.net/2013/02/02/zsh-configuration-from-the-ground-up/
## http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
## https://github.com/myfreeweb/dotfiles/blob/master/zsh/zshrc

#------------------------------------------------------------------------------
# 8. Functions
#------------------------------------------------------------------------------
## Use ctrl-z to return to paused Vim instead of 'fg<Enter>'.
## http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

##-------------------------------------------------------------------
## myIP address
## -------------------------------------------------------------------
function myip() {
  ifconfig lo0 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "lo0       : " $2}'
  ifconfig en0 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "en0 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
  ifconfig en0 | grep 'inet6 ' | sed -e 's/ / /' | awk '{print "en0 (IPv6): " $2 " " $3 " " $4 " " $5 " " $6}'
  ifconfig en1 | grep 'inet ' | sed -e 's/:/ /' | awk '{print "en1 (IPv4): " $2 " " $3 " " $4 " " $5 " " $6}'
  ifconfig en1 | grep 'inet6 ' | sed -e 's/ / /' | awk '{print "en1 (IPv6): " $2 " " $3 " " $4 " " $5 " " $6}'
}

# -------------------------------------------------------------------
# compressed file expander
# (from https://github.com/myfreeweb/zshuery/blob/master/zshuery.sh)
# -------------------------------------------------------------------
ex() {
    if [[ -f $1 ]]; then
        case $1 in
          *.tar.bz2) tar xvjf $1;;
          *.tar.gz) tar xvzf $1;;
          *.tar.xz) tar xvJf $1;;
          *.tar.lzma) tar --lzma xvf $1;;
          *.bz2) bunzip $1;;
          *.rar) unrar $1;;
          *.gz) gunzip $1;;
          *.tar) tar xvf $1;;
          *.tbz2) tar xvjf $1;;
          *.tgz) tar xvzf $1;;
          *.zip) unzip $1;;
          *.Z) uncompress $1;;
          *.7z) 7z x $1;;
          *.dmg) hdiutul mount $1;; # mount OS X disk images
          *) echo "'$1' cannot be extracted via >ex<";;
    esac
    else
        echo "'$1' is not a valid file"
    fi
}

#------------------------------------------------------------------------------
# 9. FASD & FZF
# - https://github.com/clvv/fasd
#------------------------------------------------------------------------------
eval "$(fasd --init auto)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_OPTS="--select-1 --exit-0"
export FZF_COMPLETION_TRIGGER=''

#bindkey '^T' fzf-completion
#bindkey '^I' $fzf_default_completion

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

# fasd & fzf - jump using `fasd` if argument is given, filter output of `fasd`
# using `fzf` otherwise.
unalias j 2>/dev/null
j() {
    [ $# -gt 0 ] && fasd_cd -d "$*" && return
    local dir
    dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

# fasd & fzf - use $EDITOR to edit file. Pick best matched file using `fasd`
# if argument given, else use `fzf` to filter `fasd` output.
unalias e 2>/dev/null
e() {
    [ $# -gt 0 ] && fasd -f -e ${EDITOR} "$*" && return
    local file
    file="$(fasd -Rfl "$1" | fzf -1 -0 --no-sort +m)" && ${EDITOR} "${file}" || return 1
}

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local files
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# fasd & fzf - open finder. If argument given, use `fasd` to pick the best match
# else use `fzf` to select from `fasd` results.
unalias o 2>/dev/null
o() {
    [ $# -gt 0 ] && fasd -a -e open "$*" && return
    local res
    res="$(fasd -Rla "$1" | fzf -1 -0 --no-sort +m)"
    if [[ -d "${res}" ]]; then
       open "${res}"
    else
       open "$(dirname "$res")"
    fi
}

#------------------------------------------------------------------------------
# 10. zinit
#------------------------------------------------------------------------------
### Added by zinit's installer
source '/Users/api/.zinit/bin/zinit.zsh'
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit
### End of zinit's installer chunk

zinit load zdharma/history-search-multi-word

zinit ice compile"*.lzui"
zinit load zdharma/zui
zinit light zdharma/zinit-crasis

zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-syntax-highlighting

zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure

# Local Variables:
# eval: (outline-minor-mode 1)
# outline-regexp:  "^# [0-9]+"
# End:
### End of Zinit's installer chunk
