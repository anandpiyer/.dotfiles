export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export EDITOR="emacsclient"

if [ -z "$TMUX" ]
then
    tmux attach -t TMUX || tmux new -s TMUX
fi

alias dotfiles='/usr/local/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias ll="exa -lGx"

#------------------------------------------------------------------------------
# Autojump - https://github.com/wting/autojump
# FASD - https://github.com/clvv/fasd
#------------------------------------------------------------------------------
#[ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh
eval "$(fasd --init auto)"

#------------------------------------------------------------------------------
# Colors
#------------------------------------------------------------------------------
autoload -U colors && colors
export CLICOLOR="yes"

#------------------------------------------------------------------------------
# Completion
#------------------------------------------------------------------------------
autoload -U compinit && compinit
zmodload -i zsh/complist
setopt auto_menu
setopt complete_in_word

zstyle ':completion:*' list-colors ''

#------------------------------------------------------------------------------
# History
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
# Prompt
#------------------------------------------------------------------------------
autoload -U promptinit && promptinit
prompt pure

# http://zanshin.net/2013/02/02/zsh-configuration-from-the-ground-up/
# http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
# https://github.com/myfreeweb/dotfiles/blob/master/zsh/zshrc

#------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------
# Use ctrl-z to return to paused Vim instead of 'fg<Enter>'.
# http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
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

#------------------------------------------------------------------------------
# FZF
#------------------------------------------------------------------------------
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'

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

export FZF_CTRL_T_OPTS="--select-1 --exit-0"

export FZF_COMPLETION_TRIGGER=''
bindkey '^T' fzf-completion
bindkey '^I' $fzf_default_completion

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
# Zplugin
#------------------------------------------------------------------------------
### Added by Zplugin's installer
source '/Users/api/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin's installer chunk

zplugin load zdharma/history-search-multi-word

zplugin ice compile"*.lzui"
zplugin load zdharma/zui
zplugin light zdharma/zplugin-crasis

zplugin light zsh-users/zsh-autosuggestions
zplugin light zsh-users/zsh-syntax-highlighting
