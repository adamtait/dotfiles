#!/usr/bin/env bash

__powerline() {

    # Unicode symbols
    PS_SYMBOL_DARWIN=''
    PS_SYMBOL_LINUX='$'
    PS_SYMBOL_OTHER='%'
    GIT_BRANCH_SYMBOL='⑂ '
    GIT_BRANCH_CHANGED_SYMBOL='+'
    GIT_NEED_PUSH_SYMBOL='⇡'
    GIT_NEED_PULL_SYMBOL='⇣'

    # Solarized colorscheme
    if [[ $(tput colors) -ge 256 ]] 2>/dev/null; then
        FG_BASE03="\[$(tput setaf 234)\]"
        FG_BASE02="\[$(tput setaf 235)\]"
        FG_BASE01="\[$(tput setaf 240)\]"
        FG_BASE00="\[$(tput setaf 241)\]"
        FG_BASE0="\[$(tput setaf 244)\]"
        FG_BASE1="\[$(tput setaf 245)\]"
        FG_BASE2="\[$(tput setaf 254)\]"
        FG_BASE3="\[$(tput setaf 230)\]"

        BG_BASE03="\[$(tput setab 234)\]"
        BG_BASE02="\[$(tput setab 235)\]"
        BG_BASE01="\[$(tput setab 240)\]"
        BG_BASE00="\[$(tput setab 241)\]"
        BG_BASE0="\[$(tput setab 244)\]"
        BG_BASE1="\[$(tput setab 245)\]"
        BG_BASE2="\[$(tput setab 254)\]"
        BG_BASE3="\[$(tput setab 230)\]"

        FG_YELLOW="\[$(tput setaf 136)\]"
        FG_ORANGE="\[$(tput setaf 166)\]"
        FG_RED="\[$(tput setaf 160)\]"
        FG_MAGENTA="\[$(tput setaf 125)\]"
        FG_VIOLET="\[$(tput setaf 61)\]"
        FG_BLUE="\[$(tput setaf 33)\]"
        FG_CYAN="\[$(tput setaf 37)\]"
        FG_GREEN="\[$(tput setaf 64)\]"

        BG_YELLOW="\[$(tput setab 136)\]"
        BG_ORANGE="\[$(tput setab 166)\]"
        BG_RED="\[$(tput setab 160)\]"
        BG_MAGENTA="\[$(tput setab 125)\]"
        BG_VIOLET="\[$(tput setab 61)\]"
        BG_BLUE="\[$(tput setab 33)\]"
        BG_CYAN="\[$(tput setab 37)\]"
        BG_GREEN="\[$(tput setab 64)\]"
     else
        FG_BASE03="\[$(tput setaf 8)\]"
        FG_BASE02="\[$(tput setaf 0)\]"
        FG_BASE01="\[$(tput setaf 10)\]"
        FG_BASE00="\[$(tput setaf 11)\]"
        FG_BASE0="\[$(tput setaf 12)\]"
        FG_BASE1="\[$(tput setaf 14)\]"
        FG_BASE2="\[$(tput setaf 7)\]"
        FG_BASE3="\[$(tput setaf 15)\]"

        BG_BASE03="\[$(tput setab 8)\]"
        BG_BASE02="\[$(tput setab 0)\]"
        BG_BASE01="\[$(tput setab 10)\]"
        BG_BASE00="\[$(tput setab 11)\]"
        BG_BASE0="\[$(tput setab 12)\]"
        BG_BASE1="\[$(tput setab 14)\]"
        BG_BASE2="\[$(tput setab 7)\]"
        BG_BASE3="\[$(tput setab 15)\]"

        FG_YELLOW="\[$(tput setaf 3)\]"
        FG_ORANGE="\[$(tput setaf 9)\]"
        FG_RED="\[$(tput setaf 1)\]"
        FG_MAGENTA="\[$(tput setaf 5)\]"
        FG_VIOLET="\[$(tput setaf 13)\]"
        FG_BLUE="\[$(tput setaf 4)\]"
        FG_CYAN="\[$(tput setaf 6)\]"
        FG_GREEN="\[$(tput setaf 2)\]"

        BG_YELLOW="\[$(tput setab 3)\]"
        BG_ORANGE="\[$(tput setab 9)\]"
        BG_RED="\[$(tput setab 1)\]"
        BG_MAGENTA="\[$(tput setab 5)\]"
        BG_VIOLET="\[$(tput setab 13)\]"
        BG_BLUE="\[$(tput setab 4)\]"
        BG_CYAN="\[$(tput setab 6)\]"
        BG_GREEN="\[$(tput setab 2)\]"
    fi

    DIM="\[$(tput dim)\]"
    REVERSE="\[$(tput rev)\]"
    RESET="\[$(tput sgr0)\]"
    BOLD="\[$(tput bold)\]"

    if [[ -z "$PS_SYMBOL" ]]; then
      case "$(uname)" in
          Darwin)
              PS_SYMBOL=$PS_SYMBOL_DARWIN
              ;;
          Linux)
              PS_SYMBOL=$PS_SYMBOL_LINUX
              ;;
          *)
              PS_SYMBOL=$PS_SYMBOL_OTHER
      esac
    fi

    __git_info() { 
        [ -x "$(which git)" ] || return    # git not found

        local git_eng="env LANG=C git"   # force git output in English to make our work easier
        # get current branch name or short SHA1 hash for detached head
        local branch="$($git_eng symbolic-ref --short HEAD 2>/dev/null || $git_eng describe --tags --always 2>/dev/null)"
        [ -n "$branch" ] || return  # git branch not found

        local marks

        # branch is modified?
        [ -n "$($git_eng status --porcelain)" ] && marks+=" $GIT_BRANCH_CHANGED_SYMBOL"

        # how many commits local branch is ahead/behind of remote?
        local stat="$($git_eng status --porcelain --branch | grep '^##' | grep -o '\[.\+\]$')"
        local aheadN="$(echo $stat | grep -o 'ahead [[:digit:]]\+' | grep -o '[[:digit:]]\+')"
        local behindN="$(echo $stat | grep -o 'behind [[:digit:]]\+' | grep -o '[[:digit:]]\+')"
        [ -n "$aheadN" ] && marks+=" $GIT_NEED_PUSH_SYMBOL$aheadN"
        [ -n "$behindN" ] && marks+=" $GIT_NEED_PULL_SYMBOL$behindN"

        # print the git branch segment without a trailing newline
        printf " $GIT_BRANCH_SYMBOL$branch$marks "
    }

    ps1() {
        # Check the exit code of the previous command and display different
        # colors in the prompt accordingly. 
        if [ $? -eq 0 ]; then
            local BG_EXIT="$BG_GREEN"
        else
            local BG_EXIT="$BG_RED"
        fi

        PS1="$BG_BASE1$FG_BASE3 \w $RESET"
        # Bash by default expands the content of PS1 unless promptvars is disabled.
        # We must use another layer of reference to prevent expanding any user
        # provided strings, which would cause security issues.
        # POC: https://github.com/njhartwell/pw3nage
        # Related fix in git-bash: https://github.com/git/git/blob/9d77b0405ce6b471cb5ce3a904368fc25e55643d/contrib/completion/git-prompt.sh#L324
        if shopt -q promptvars; then
            __powerline_git_info="$(__git_info)"
            PS1+="$BG_BLUE$FG_BASE3\${__powerline_git_info}$RESET"
        else
            # promptvars is disabled. Avoid creating unnecessary env var.
            PS1+="$BG_BLUE$FG_BASE3$(__git_info)$RESET"
        fi
        PS1+="$BG_EXIT$FG_BASE3 $PS_SYMBOL $RESET "
    }

    PROMPT_COMMAND=ps1
}

__powerline
unset __powerline

