【ソフト名】UncrustifyRS
【著作権者】筑木　真志 (Shinji_dot_Chikugi_at_nifty_dot_com)
【制 作 日】2010/12/26
【種　　別】開発支援
【連 絡 先】Shinji.Chikugi@nifty.com
【配 布 元】http://a7m.sakura.ne.jp/
【転　　載】転載前要連絡
【登 録 名】UncrustifyRS100.7z
【圧縮形式】7Zip
【動作環境】Windows XP/Vista/7/Server 2003/Server 2008上で動作する
            RAD Studio/C++Builder XE
【開発環境】RAD Stuio XE
―――――――――――――――――――――――――――――――――――――
≪著作権および免責事項≫

　本ソフトはフリーソフトです。自由にご使用ください。なお，著作権は作者である筑木真志が保有しています。

　このソフトウェアを使用したことによって生じたすべての障害・損害・不具合等に関しては、私と私の関係者および私の所属するいかなる団体・組織とも、一切の責任を負いません。各自の責任においてご使用ください。

・はじめに
RAD StudioのIDEを拡張して、Uncrustifyのような外部コードフォーマッターを適用するツールです。

・ファイル構成
UncrustifyRS.bpl：アドオンパッケージ本体
ReadMe.txt：このファイル
Source\：Delphi/RAD Studio XE用ソースコード一式
uncrustify-0.57-win32\：Uncrustifyのバイナリ一式

・インストール方法
RAD Studioの[コンポーネント|パッケージのインストール]を選択し、[追加]をクリックします。
ファイル選択ダイアログが表示されますので、UncrustifyRS.bplを選択してください。

・アンインストール方法
RAD Studioの[コンポーネント|パッケージのインストール]を選択します。
「設計時パッケージ」一覧から「Source Code Beautifier for RAD Studio/C++Builder」を選択し、[削除]をクリックします。
プログラムオプションはHKEY_CURRENT_USER\Software\dragonhouse-software.jp\UncrustifyRSに保存されますので、削除してください。


・使い方
UncrustifyRSをインストールすると、RAD






・配布ライセンス
UncrustifyRSそのものの配布ライセンスはMPL 1.1です。
UncrustifyRS is released in accordance with the MPL 1.1 license. To get your own copy or read it, go to http://www.mozilla.org/MPL/MPL-1.1.html. 

uncrustifyのオリジナルバイナリはhttp://uncrustify.sourceforge.net/より入手できます。配布条件はオリジナルに従ってください。

uCommandLineUnit.pasは単なるオープンソースなので自由に使ってくださいとのことです。
オリジナル：http://delfusa.main.jp/delfusafloor/opensource/delfusa_library_f.html
改良版：http://mrxray.on.coocan.jp/Delphi/plSamples/552_PipeRedirect.htm
元著作者のDelfusaさま（http://delfusa.main.jp/delfusafloor/index.shtml）、改良者のMr.XRAYさま（http://mrxray.on.coocan.jp/）に感謝!!

ソースコードの一部にGExperts（http://www.gexperts.org/）のソースを使用しています。その部分の配布条件はGExpertsのLicense Agreement（http://www.gexperts.org/license.html）に従ってください。

・FAQ
Q1. RAD Studioってコードフォーマッターがあるよね？
	C++のコーディングスタイルは千差万別で、好みが大きく分かれます。
	RAD StudioのC++用コードフォーマッターの機能では、細かいところまで制御出来ないので作りました。

Q2. システムデフォルトとUTF-8以外のエンコーディングは使えないの?
	はい。現時点で8ビット長限定です。
	てか、UTF-16とかUTF-32でソースコードは書かないでしょ？(多分)

Q2. ビルド方法は？
	RAD Stuio XEかDelphi XEが必要です。(^^ゞ

Q3. C++Builder用のツールなのに、なぜ、C++Builderではビルド出来ないの?
	Open Tools APIを使う上では、ぶっちゃけ、C++BuilderよりDelphiのほうが圧倒的に楽。
	あと、なぜか自分の環境ではC++Builder上でOpen Tools APIを使ったアプリのデバッグが出来なかったので。
	残念ながらそのままではビルド出来ないのですが、C++Builderで新規パッケージプロジェクトを作成して、Delphiのソースコードをプロジェクトに追加すればC++Builder単体でビルド出来るかも。
	個人的に、C++BuilderでJVCLなどのオープンソースコンポーネントを使うのであれば、Delphiを併用できる統合版のRAD Studioマジおすすめ。

Q4. 何故、ライセンスはMPL 1.1なの？
	理由はコードの一部にGExperts（http://www.gexperts.org/）のソースを使っていて、そのライセンス要件が「ソースを使用した場合は、成果物を何らかのオープンソースライセンスで公開すること」（意訳）となっているからです。
	GPL/LGPLは動的／静的リンクをした場合にライセンス的に矛盾が発生する可能性があること。
	MIT/BSDライセンスはクローズドソースにコードが流用出来てしまうので、GExpertsのライセンスに反してしまう可能性があること。
	よって、改変して公開した場合は「改変部のソースコードの公開を強要する」MPLが一番バランスがよいと思ったので、MPLにしました。
	まぁ、ソースコードのライセンスは著作権者の権利が及ぶところにしか適用できないから、自作以外の部分はそんなに神経質になる必要はないんだけど。

Q5. 今後の改良予定は？
	欲しい機能は実現できたので特に予定はないけれど、UniversalIndentGUIのようなRAD Studioと連動できるGUIフロントエンドは欲しいな…。
	汎用化して任意のフィルタプログラムをコードエディタに適用させるようにするとか。

・履歴
2010/12/26 Version 1.00
	最初のリリースバージョン

以上
