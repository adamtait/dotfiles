#!/bin/sh

# Google fonts API key: AIzaSyBms6HpvhPRKRqLf2cWz9faRMrqzlhi6Ag


FONTS_DIR=$HOME/Library/Fonts


### --------------- Anonymous Pro ---------------
if [[ ! -e $FONTS_DIR/AnonymousPro.regular.ttf ]]; then
    echo "--- installing Anonymous Pro"

    curl http://fonts.gstatic.com/s/anonymouspro/v10/Zhfjj_gat3waL4JSju74E-V_5zh5b-_HiooIRUBwn1A.ttf > $FONTS_DIR/AnonymousPro.regular.ttf
    curl http://fonts.gstatic.com/s/anonymouspro/v10/q0u6LFHwttnT_69euiDbWKwIsuKDCXG0NQm7BvAgx-c.ttf > $FONTS_DIR/AnonymousPro.italic.ttf
    curl http://fonts.gstatic.com/s/anonymouspro/v10/WDf5lZYgdmmKhO8E1AQud--Cz_5MeePnXDAcLNWyBME.ttf > $FONTS_DIR/AnonymousPro.700.ttf
    curl http://fonts.gstatic.com/s/anonymouspro/v10/_fVr_XGln-cetWSUc-JpfA1LL9bfs7wyIp6F8OC9RxA.ttf > $FONTS_DIR/AnonymousPro.700italic.ttf
fi


### --------------- Roboto Mono ---------------
if [[ ! -e $FONTS_DIR/RobotoMono.regular.ttf ]]; then
    echo "--- installing Roboto Mono (terminal font)"

    curl http://fonts.gstatic.com/s/robotomono/v4/aOIeRp72J9_Hp_8KwQ9M-YAWxXGWZ3yJw6KhWS7MxOk.ttf > $FONTS_DIR/RobotoMono.100.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/rqQ1zSE-ZGCKVZgew-A9dgyDtfpXZi-8rXUZYR4dumU.ttf > $FONTS_DIR/RobotoMono.100italic.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/N4duVc9C58uwPiY8_59Fzy9-WlPSxbfiI49GsXo3q0g.ttf > $FONTS_DIR/RobotoMono.300.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/1OsMuiiO6FCF2x67vzDKA2o9eWDfYYxG3A176Zl7aIg.ttf > $FONTS_DIR/RobotoMono.300italic.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/eJ4cxQe85Lo39t-LVoKa26CWcynf_cDxXwCLxiixG1c.ttf > $FONTS_DIR/RobotoMono.regular.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/mE0EPT_93c7f86_WQexR3EeOrDcLawS7-ssYqLr2Xp4.ttf > $FONTS_DIR/RobotoMono.italic.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/N4duVc9C58uwPiY8_59Fz8CNfqCYlB_eIx7H1TVXe60.ttf > $FONTS_DIR/RobotoMono.500.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/1OsMuiiO6FCF2x67vzDKA2nWRcJAYo5PSCx8UfGMHCI.ttf > $FONTS_DIR/RobotoMono.500italic.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/N4duVc9C58uwPiY8_59Fz3e1Pd76Vl7zRpE7NLJQ7XU.ttf > $FONTS_DIR/RobotoMono.700.ttf
    curl http://fonts.gstatic.com/s/robotomono/v4/1OsMuiiO6FCF2x67vzDKA8_zJjSACmk0BRPxQqhnNLU.ttf > $FONTS_DIR/RobotoMono.700italic.ttf
fi


### --------------- Hack ---------------
if [[ ! -e $FONTS_DIR/Hack-Regular.ttf ]]; then
    echo "--- installing Hack"   # unfortunately, not available on Google Fonts

    OLD_DIR=`pwd`
    TEMP_DIR=/tmp/hack-font
    mkdir $TEMP_DIR
    cd $TEMP_DIR
    curl -s https://api.github.com/repos/source-foundry/Hack/releases/latest \
        | grep "browser_download_url.*ttf.tar.gz" \
        | cut -d : -f 2,3 \
        | tr -d \" \
        | wget -qi -
    tar -zxmf Hack*
    cp *.ttf $FONTS_DIR/
    cd $OLD_DIR
    rm -rf $TEMP_DIR
fi
