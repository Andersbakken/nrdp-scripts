
#emacs build
emake()
{
    "emacsedit.sh" -m -n "${1}"
}

#make wrapper
make()
{
    "ubermake.sh" "$@"
}

#gdb wrapper
gdb()
{
    EDITOR="emacsedit.sh -n" `which gdb` "$@"
}

#emacs diff
ediff()
{
    f="ediff-files"
    if [ "$1" = "-b" ]; then
        f="sam-ediff-binary-files"
        shift
    fi
    "emacsedit.sh" -r -n "($f \"${1}\" \"${2}\")"
}

#ecd
ecd()
{
    if [ -n "$1" ]; then
        emacsedit.sh -n "$1"
    else
        cdd "@"
    fi
}

#emacs tail
etail()
{
    "emacsedit.sh" -n -t "$1"
}

#global
findsym() {
    ROOT=`findancestor GPATH 2>&1 || findancestor GTAGS 2>&1|| findancestor GRTAGS 2>&1`
    if [ -z "$ROOT" ]; then
        echo "No gtags found!"
    else
        ROOT=`dirname $ROOT`
        GLOBAL_OPTS="-x"
        if [ "$1" = "-h" ]; then
            echo "findsym <option> <find>"
            echo "Options:"
            echo
            echo "-symbol: <find> a symbol."
            echo "-caller: <find> a caller of symbol."
            echo "-tag: <find> a referencd string."
            echo "-file: <find> a file."
            return 1
        elif [ "$1" = "-symbol" ] || [ "$1" = "-s" ]; then
            shift
            GLOBAL_OPTS="-xs"
        elif [ "$1" = "-caller" ] || [ "$1" = "-r" ] || [ "$1" = "-reference" ]; then
            shift
            GLOBAL_OPTS="-xr"
        elif [ "$1" = "-tag" ] || [ "$1" = "-t" ]; then
            shift
            GLOBAL_OPTS="-x"
        elif [ "$1" = "-file" ] || [ "$1" = "-f" ]; then
            shift
            GLOBAL_OPTS="-xPo"
        fi
        #echo "$ROOT :: $GLOBAL_OPTS :: $@ "
        SYM=`(cdo "$ROOT" && choose.pl -a -x "global $GLOBAL_OPTS $@")`
        if [ -n "$SYM" ]; then
            echo "$SYM"
            FILE=`echo $SYM | awk '{print $3}'`
            LINE=`echo $SYM | awk '{print $2}'`
            edit "$ROOT/$FILE:$LINE"
        else
            echo "Not found!"
        fi
    fi
}

block-icecream() {
    [ -e "/etc/icecc/icecc.conf" ] && . /etc/icecc/icecc.conf
    [ -z "$ICECC_SCHEDULER_HOST" ] && ICECC_SCHEDULER_HOST="lgux-pnavarro3.corp.netflix.com"
    echo blockcs "$1" | nc "$ICECC_SCHEDULER_HOST" 8766
}

complete-netflix ()
{
    local cur=${COMP_WORDS[COMP_CWORD]}
    local valueopts="--write-data-path= -R= --resources-path= -x= --config-file= --cache-path= --log-cache-capacity= --ui-cache-capacity= -D= --define-environment= -l= --loglevel= -L= --logfile= --cached-dns-domains= --telnet-port= -M= --mutex-lock-time-tracking-interval= --binary-hash= --terminal-log-header-color= --terminal-trace-color= --terminal-info-color= --terminal-warning-color= --terminal-error-color= --terminal-fatal-color= -S= --nccp-url= -C= --nccp-cert= -T= --trace-areas= --nbp-threadpool-threadcount= --video-bitrate-ranges= --min-audio-bitrate= --max-audio-bitrate= --num-of-images-per-bifcache= --num-of-bifCache= --max-available-memory-for-bifcache= --num-of-reuse-socket-connection= --inst-thread-throttle-ms= --inst-thread-throttle-events= --inst-max-buffer-size= --inst-max-buffer-count= --inst-max-buffer-time= --inst-post-url= -U= --ui-url= -b= --boot-url= -B= --appboot-url= --appboot-params= -J= --js-opts= -Q= --ui-query-string= -c= --ui-cert= --screen-width= --screen-height= --ui-width= --ui-height= --ui-aspect-ratio= -p= --nbpd-port= -I= --idfile= --appboot-key= --dpi-x509-cert= --dpi-x509-key= --dpi-language= --dpi-friendlyname= --dpi-videobufferpoolsize= --dpi-audiobufferpoolsize= --dpi-videoscreenwidth= --dpi-videoscreenheight= --mdx-interface= --user-name= --user-password= --debug-flags= --surface-cache-capacity= --surface-cache-playback-capacity= --disk-cache-capacity= --disk-cache-max-pending= --disk-cache-max-writes= --resource-cache-capacity= --disk-cache-path= --fps-target= --key-repeat-style= --key-repeat-interval= --key-repeat-delay= --animation-image-threads= --image-threads= --garbage-collect-timeout= --inject-js= --alpha-format= --opaque-format= --default-network-timeout= --default-network-connect-timeout= --text-font-path= --default-locale= --font-face-cache-capacity= --font-glyph-cache-width= --font-glyph-cache-height="
    local nonvalueopts="-h --help -Z --dump-config -z --dump --disable-logcolor --disable-logabsolute --disable-dnscache --disable-telnet --disable-mutex-stack -t --write-enabled-trace-only -P --disable-ssl-peer-verification --enable-js-assert --enable-reuse-socket-connection --inst-switched-events-initially-on --inst-switched-events-always-on --inst-disable-log --inst-ignore-config --inst-print --inst-nts-post --inst-stash --inst-stash-initially-off -f --show-fps --no-mdx -u --disable-updated-truststore -X --disable-url-filter -i --nbpd-all-interfaces --disable-nbpd --dpi-mgk --dpi-has-pointer --dpi-has-keyboard --dpi-has-osk --dpi-support-videoseamlessswitch --dpi-support-3Dvideoseamlessswitch --dpi-support-videoskipback --dpi-support-3Dvideoskipback --dpi-support-videoovergraphics --dpi-support-2DVideoResize --dpi-support-3DVideoResize --dpi-support-2DVideoResizeDuringPlayback --dpi-support-3DVideoResizeDuringPlayback --dpi-support-video2DResizeAnimation --dpi-support-video3DResizeAnimation --dpi-support-DrmPlaybackTransition --cache-animations --no-libedit -K --send-secure-cookies --no-splash-screen --no-platform-surface-decoders"
    COMPREPLY=(`compgen -W "$valueopts $nonvalueopts" -- $cur`)
}
if [ -n "$BASH" ]; then
    complete -F complete-netflix -o nospace netflix ./netflix
fi
