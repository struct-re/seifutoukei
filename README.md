seifutoukei
===========

Ｒから[「政府統計の総合窓口」](http://www.e-stat.go.jp/api)にアクセスできるパッケージです。政府統計を検索し、データを data.frame に自動変換して取得します。

Access [Japanese official statistics open
data](http://www.e-stat.go.jp/api/) from R.

### クレジット表示 / Disclaimer

このソフトウェアは、政府統計総合窓口(e-Stat)のAPI機能を使用していますが、ソフトウェアの内容は国によって保証されたものではありません。

This is experimental software intended for use in academic
research. It is being published under the terms of the GNU Public
License (version 2).

Although this software accesses an open service run by the Japanese
government, its author has no connection with the Japanese government,
which also provides no guarantees regarding the output produced by the
software.

### 更新情報 / Version history

#### v0.2 (2014-11-26)

- 政府統計の総合窓口(e-Stat)の機能としての本格運用開始に伴い，サーバーURIを statdb.nstac.go.jp から api.e-stat.go.jp に変更
- Switched from the experimental server at statdb.nstac.go.jp to the new
e-stat API server

#### v0.1 (2014-04-03)

Initial release / 初版

## インストールと初期設定 / Intallation and setup

````R
library(devtools)
install_github('struct-re/seifutoukei')

## recommended settings, can be added to your ~/.Rprofile
options(
    seifutoukei.application.id = "...",
    seifutoukei.http.cache = TRUE,
    seifutoukei.http.skip.confirmation = TRUE
    )
````

当パッケージを利用するには「アプリケーションID」が必要です。[利用登録](http://www.e-stat.go.jp/api/regist-login/)の上、アプリケーションIDを取得してください。以上のように option として保存できます。

To use the functions in this package, you will need to obtain an API
key (called an 'application ID') by [registering
here](http://www.e-stat.go.jp/api/regist-login/). You can then save it
as an option.

## 利用方法 / Usage 

````R
library(seifutoukei)

# 公開統計のリストを出力する
# list surveys with available data
stlistsurveys()

# 統計表を検索する
# Search for resources (resources are typically summary tables)
stfind(keywords)
stfind(survey.name = "...")
stfind(survey.date = "yyyy")
stfind(survey.date = "yyyy-mm")
stfind(survey.date = c(from="yyyy-mm", to="yyyy-mm"))
stfind(keywords, survey.name = "...", survey.date = c(from="yyyy-mm", to="yyyy-mm"))

# 統計表のメタ情報を閲覧する
# View resource metadata (names of variables and classes)
meta <- stgetmetadata(resource.id) ## resource.id は統計表番号
meta$table
meta$classes

# 統計表からデータを取得する
# Download data from resource
response <- stgetdata(resource.id, filters = list(
    '...' = c('...', '...'),
    '...' = '...'
))

# コードを日本語の変数名・分類項目に変換する
# Human-readable variable names and factor labels
humanise(response)

````

パッケージの全関数はヘルプが付いていますが、現在英語のみとなっています。

Online help in English is available for all package functions.

````R
help(stfind)
help(stgetmetadata)
help(stgetdata)
````

## 活用例 / Usage example

````R
metadata <- stgetmetadata('0003038586')
str(metadata)
## List of 2
##  $ table  :List of 6
##   ..$ STAT_NAME      : chr "国勢調査"
##   ..$ GOV_ORG        : chr "総務省"
##   ..$ STATISTICS_NAME: chr "平成22年国勢調査 人口等基本集計（男女・年齢・配偶関係，世帯の構成，住居の状態など）"
##   ..$ TITLE          : chr "人口，人口増減，面積及び人口密度 全国，市部・郡部，都道府県，市部・郡部，支庁，郡計，市区町村・旧市町村，全域・人口集中地区"
##   ..$ SURVEY_DATE    : chr "201010"
##   ..$ RESOURCE_ID    : chr "0003038586"
##  $ classes:'data.frame':	4517 obs. of  6 variables:
##   ..$ class.id  : chr [1:4517] "tab" "tab" "tab" "tab" ...
##   ..$ class.name: chr [1:4517] "表章項目" "表章項目" "表章項目" "表章項目" ...
##   ..$ code      : chr [1:4517] "020" "100" "101" "102" ...
##   ..$ name      : chr [1:4517] "人口" "組替人口（平成17年）" "平成17年～22年の人口増減数" "平成17年～22年の人口増減率" ...
##   ..$ level     : chr [1:4517] "" "" "" "" ...
##   ..$ unit      : chr [1:4517] "人" "人" "人" "％" ...

unique(metadata$classes$class.name)
## [1] "表章項目"               "全域・人口集中地区2010" "地域（2010）"          
## [4] "時間軸(年次)"          
unique(with(metadata$classes, name[class.name == '表章項目']))
## [1] "人口"                       "組替人口（平成17年）"      
## [3] "平成17年～22年の人口増減数" "平成17年～22年の人口増減率"
## [5] "面積"                       "人口密度"                  

filters <- list(
    areas                    = 13101),
    '表章項目'               = c('人口', '面積', '人口密度'),
    '全域・人口集中地区2010' = '全域'
    )

response <- stgetdata('0003038586', filters)

names(response)
## [1] "metadata"  "footnotes" "data"     

response$footnotes
##   marker            content
## 1    *** 当該数値がないもの
## 2      - 当該数値がないもの


response$data
##   tab cat01  area       time   unit    value
## 1 020 00710 13101 2010000000     人 47115.00
## 2 103 00710 13101 2010000000 平方km    11.64
## 3 104 00710 13101 2010000000         4047.70

humanise(response)
##   表章項目 全域・人口集中地区2010 地域（2010） 時間軸(年次)   unit    value     id
## 1     人口                   全域     千代田区       2010年     人 47115.00  13101
## 2     面積                   全域     千代田区       2010年 平方km    11.64  13101
## 3 人口密度                   全域     千代田区       2010年         4047.70  13101

````
