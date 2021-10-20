#!/bin/bash
# DLisp 性能測定用コード
# 途中結果は tmp ディレクトリ内にファイルを作ってそこへ書き込む

# エラーが起きた時は終了するようにする
set -eu

################################################################################

DLISP='/Users/sano/work/dlisp/_build/install/default/bin/dlisp'


################################################################################

# スクリプトが置かれている場所の一段上のディレクトリへ移動する
# プロジェクトが root_of_the_project/script/this_file.sh のようになっていることを前提にして，
# プロジェクトのルートディレクトリへ移動している
# （そうでないと実行できない・実行しづらいアプリも想定）
cd "`dirname "$0"`"'/..' 


# 中間データの出力用に temporary directory を作る
# -p で，元々その directory が存在していた場合もエラーを吐かなくなる
mkdir -p tmp


> tmp/dlisp_result.txt
> tmp/sheme_result_append_map.txt
> tmp/sheme_result_builtin_map.txt
> tmp/length.txt

################################################################################
echo "list length, dlisp, scheme (map with append), scheme (default map)" > tmp/result.txt 

for n in $(seq -f "%.0f" 5000 5000 60000); do
    echo $n
    touch tmp/prog.lisp

    echo "$n" >> tmp/length.txt
    
    sed "s/LENGTH/$n/g" benchmark/map1.lisp | \
        sed "s/MAP/push_back_map/g" | \
            sed "s/APPEND/++/g" \
                > tmp/prog.lisp
    gtime -f "%e" $DLISP tmp/prog.lisp 2>> tmp/dlisp_result.txt 1>/dev/null

    sed "s/LENGTH/$n/g" benchmark/map1.lisp | \
        sed "s/MAP/push_back_map/g" | \
            sed "s/APPEND/append/g" \
            > tmp/prog.lisp
    
    gtime -f "%e" scheme --quiet < tmp/prog.lisp 2>> tmp/sheme_result_append_map.txt

    sed "s/LENGTH/$n/g" benchmark/map1.lisp | \
        sed "s/MAP/map/g" | \
            sed "s/APPEND/append/g" \
            > tmp/prog.lisp
    gtime -f "%e" scheme --quiet < tmp/prog.lisp 2>> tmp/sheme_result_builtin_map.txt

done


echo '>>>> result' >&2
echo ""
cat tmp/result.txt
# 「paste -d 区切り文字 ファイル1 ... ファイルn」で区切り文字で区切ってファイルの内容を出力する
paste -d ',' tmp/length.txt tmp/dlisp_result.txt tmp/sheme_result_append_map.txt tmp/sheme_result_builtin_map.txt 


# rm -rf tmp


