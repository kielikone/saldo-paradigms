# Saldo-inflector

Saldo inflector is a morphological inflector for Swedish. It is built from the Haskell source code [released by Språkbanken](https://spraakbanken.gu.se/resurser/saldo). (Scroll to the bottom of the page and look in the archive labeled SALDO v.1.0)

The morphological paradigms for Saldo were carved out from the source code and adapted to build (hopefully reproducibly) using a modern Haskell stack. Glue code was written such that these paradigms can be used from Python:

```python
>>> import saldo
>>> saldo.paradigm("nn_1u_ros", "ros")
{'sg indef nom': ['ros'], 'sg indef gen': ['ros'], 'sg def nom': ['rosen'], 'sg def gen': ['rosens'], 'pl indef nom': ['rosor'], 'pl indef gen': ['rosors'], 'pl def nom': ['rosorna'], 'pl def gen': ['rosornas'], 'comp': ['ros-', 'ros']}
>>> saldo.paradigm("nn_0n_cesium", "cesium")["sg def nom"]
['cesiet', 'cesiumet', 'cesium']
```

This library supports amd64 and aarch64 Linux and macOS. There is no fundamental reason as to why it couldn't support Windows, the effort to build the `dll` has just not been expended as of yet.

## Installation

### Pypi

Simply `pip install saldo-inflector`.

### Building for Linux

Prerequisite: a working Docker environment and `make`. Do

```shell
make docker-python-linux-amd64
```

or

```shell
make docker-python-linux-aarch64
```

depending on which architecture you wish to build for. The resultant `.whl` should pop into `saldo-python/dist`. It can be installed eg. with pip like any other `.whl`:

```shell
pip install path/to/the.whl
```

NB: The docker command copies the files from the subdirectories and the Makefile over to the container. If you are building both inside and outside of the docker image (both for Linux and macOS), you should probably do Linux first so as to not copy errant files created by the non-containerized build over.

### Building for macOS

Prerequisite: `make`, [`stack`](https://docs.haskellstack.org/en/stable/) and `python3` environment (venv should be the easiest) with recent versions of `build` and `wheel`.

Do `make`. Cross-building between different macOS architectures isn't supported as of now.

## Usage

Import the `paradigm(paradigm_name, word)` function from `saldo`. `paradigm_name` is one of the paradigms listed below and `word` is an arbitrary lemma to return the paradigm of. The function returns a `dict[str, list[str]]` if the paradigm is found and `None` otherwise.

## Paradigms

The following paradigms are supported (ostensibly the ones from SALDO v.1.0). Individual forms vary between paradigms.

| Paradigm name                  | Example phrase            |
|--------------------------------|---------------------------|
| ab\_1\_fort                      | fort                      |
| ab\_i\_inte                      | inte                      |
| ab\_is\_främst                   | främst                    |
| aba\_i\_dvs                      | dvs                       |
| abm\_i\_till\_exempel             | till exempel              |
| av\_0\_lastgammal                | lastgammal                |
| av\_0\_medelstor                 | medelstor                 |
| av\_1\_akut                      | akut                      |
| av\_1\_blå                       | blå                       |
| av\_1\_bred                      | bred                      |
| av\_1\_brydd                     | brydd                     |
| av\_1\_grann                     | grann                     |
| av\_1\_gul                       | gul                       |
| av\_1\_lat                       | lat                       |
| av\_1\_ny                        | ny                        |
| av\_1\_rund                      | rund                      |
| av\_1\_stum                      | stum                      |
| av\_1\_utbrunnen                 | utbrunnen                 |
| av\_i\_diverse                   | diverse                   |
| in\_i\_aj                        | aj                        |
| inm\_i\_aja\_baja                 | aja baja                  |
| kn\_i\_och                       | och                       |
| nl\_g\_halvannan                 | halvannan                 |
| nl\_g\_tu                        | tu                        |
| nl\_i\_i                         | i                         |
| nl\_n\_1                         | 1                         |
| nl\_n\_elva                      | elva                      |
| nl\_n\_en                        | en                        |
| nl\_n\_fem                       | fem                       |
| nl\_n\_fyra                      | fyra                      |
| nl\_n\_hundra                    | hundra                    |
| nl\_n\_sex                       | sex                       |
| nl\_n\_tio                       | tio                       |
| nl\_n\_tolv                      | tolv                      |
| nl\_n\_tre                       | tre                       |
| nl\_n\_två                       | två                       |
| nl\_n\_åtta                      | åtta                      |
| nn\_0n\_babbel                   | babbel                    |
| nn\_0n\_cesium                   | cesium                    |
| nn\_0n\_kaos                     | kaos                      |
| nn\_0n\_oväsen                   | oväsen                    |
| nn\_0n\_toapapper                | toapapper                 |
| nn\_0n\_raseri                   | raseri                    |
| nn\_0n\_skum                     | skum                      |
| nn\_0n\_syre                     | syre                      |
| nn\_0u\_akribi                   | akribi                    |
| nn\_0u\_antimateria              | antimateria               |
| nn\_0u\_frid                     | frid                      |
| nn\_0u\_samverkan                | samverkan                 |
| nn\_0u\_skam                     | skam                      |
| nn\_0u\_tro                      | tro                       |
| nn\_0v\_bikarbonat               | bikarbonat                |
| nn\_0v\_manna                    | manna                     |
| nn\_1u\_ros                      | ros                       |
| nn\_2u\_afton                    | afton                     |
| nn\_2u\_bro                      | bro                       |
| nn\_2u\_dotter                   | dotter                    |
| nn\_2u\_fordran                  | fordran                   |
| nn\_2u\_verkan                   | verkan                    |
| nn\_2u\_fröken                   | fröken                    |
| nn\_2u\_karl                     | karl                      |
| nn\_2u\_moder                    | moder                     |
| nn\_2u\_morgon                   | morgon                    |
| nn\_2u\_mun                      | mun                       |
| nn\_3n\_gift                     | gift                      |
| nn\_3n\_land                     | land                      |
| nn\_3n\_museum                   | museum                    |
| nn\_3u\_bok                      | bok                       |
| nn\_3u\_fot                      | fot                       |
| nn\_3u\_get                      | get                       |
| nn\_3u\_historia                 | historia                  |
| nn\_3u\_kavaljer                 | kavaljer                  |
| nn\_3u\_motor                    | motor                     |
| nn\_3u\_son                      | son                       |
| nn\_3u\_stad                     | stad                      |
| nn\_3u\_tång                     | tång                      |
| nn\_3u\_vän                      | vän                       |
| nn\_3v\_flanell                  | flanell                   |
| nn\_4u\_bonde                    | bonde                     |
| nn\_5n\_ansikte                  | ansikte                   |
| nn\_5n\_bo                       | bo                        |
| nn\_5u\_anmodan                  | anmodan                   |
| nn\_6n\_aber                     | aber                      |
| nn\_6n\_blad                     | blad                      |
| nn\_6n\_foder                    | foder                     |
| nn\_6n\_frx                      | frx                       |
| nn\_6n\_papper                   | papper                    |
| nn\_6n\_rum                      | rum                       |
| nn\_6n\_träd                     | träd                      |
| nn\_6u\_akademiker               | akademiker                |
| nn\_6u\_broder                   | broder                    |
| nn\_6u\_fader                    | fader                     |
| nn\_6u\_kammare                  | kammare                   |
| nn\_6u\_kikare                   | kikare                    |
| nn\_6u\_mus                      | mus                       |
| nn\_6u\_vaktman                  | vaktman                   |
| nn\_6v\_borst                    | borst                     |
| nn\_7u\_hit                      | hit                       |
| nn\_7u\_ranger                   | ranger                    |
| nn\_in\_vaj                      | vaj                       |
| nn\_iu\_vank                     | vank                      |
| nn\_iv\_hum                      | hum                       |
| nn\_on\_öga                      | öga                       |
| nn\_ou\_officer                  | officer                   |
| nn\_vn\_alfa\_abc                 | a                         |
| nn\_vn\_garn                     | garn                      |
| nn\_vn\_huvud                    | huvud                     |
| nn\_vn\_kvantum                  | kvantum                   |
| nn\_vn\_neutrum                  | neutrum                   |
| nn\_vn\_spektrum                 | spektrum                  |
| nn\_vu\_blinker                  | blinker                   |
| nn\_vu\_cyklamen                 | cyklamen                  |
| nn\_vu\_dress                    | dress                     |
| nn\_vu\_hambo                    | hambo                     |
| nn\_vu\_kaliber                  | kaliber                   |
| nn\_vu\_playboy                  | playboy                   |
| nn\_vu\_trio                     | trio                      |
| nn\_vv\_borr                     | borr                      |
| nn\_vv\_test                     | test                      |
| nna\_iu\_dr                      | dr                        |
| nna\_iv\_nxn                     | nxn                       |
| nna\_6u\_m                       | m                         |
| pm\_fph\_alice                   | alice                     |
| pm\_fph\_karin                   | karin                     |
| pm\_fph\_lisa                    | lisa                      |
| pm\_fpm\_idun                    | idun                      |
| pm\_hph\_berg                    | berg                      |
| pm\_hph\_svensson                | svensson                  |
| pm\_mph\_ansgar                  | ansgar                    |
| pm\_mph\_bo                      | bo                        |
| pm\_mph\_lars                    | lars                      |
| pm\_mph\_sture                   | sture                     |
| pm\_mpm\_oden                    | oden                      |
| pm\_nlf\_kreml                   | kreml                     |
| pm\_nlg\_delhi                   | delhi                     |
| pm\_nlg\_eurasien                | eurasien                  |
| pm\_nlg\_göteborg                | göteborg                  |
| pm\_nlp\_bender                  | bender                    |
| pm\_nlp\_sverige                 | sverige                   |
| pm\_nog\_volvo                   | volvo                     |
| pm\_nop\_centern                 | centern                   |
| pm\_plg\_alperna                 | alperna                   |
| pm\_uag\_saab                    | saab                      |
| pm\_ula\_månen                   | månen                     |
| pm\_ulg\_fyris                   | fyris                     |
| pm\_uwb\_hemsöborna              | hemsöborna                |
| pm\_uwc\_faust                   | faust                     |
| pm\_uwn\_aftonbladet             | aftonbladet               |
| pm\_vlf\_globen                  | globen                    |
| pma\_nog\_fn                     | fn                        |
| pma\_nop\_cuf                    | cuf                       |
| pmm\_h0ph\_de\_saussure           | de saussure               |
| pmm\_m0ph\_bo\_ek                 | bo ek                     |
| pmm\_n0lg\_new\_delhi             | new delhi                 |
| pmm\_n0lg\_svarta\_havet          | svarta havet              |
| pmm\_n0lp\_sri\_lanka             | sri lanka                 |
| pmm\_n0oe\_göteborgs\_universitet | göteborgs universitet     |
| pmm\_n0og\_nordiska\_rådet        | nordiska rådet            |
| pmm\_u0lg\_torne\_älv             | torne älv                 |
| pmm\_u0wb\_det\_går\_an            | Quo vadis?                |
| pmm\_n0wm\_ring\_p1               | Ring P1                   |
| pmm\_u0ec\_alla\_hjärtans\_dag     | alla hjärtans dag         |
| pmm\_u0og\_svenska\_akademien     | Svenska Akademien         |
| pn\_i\_man                       | man                       |
| pnm\_x1\_inte\_ett\_dugg           | inte ett dugg             |
| pp\_i\_i                         | i                         |
| vb\_1a\_laga                     | laga                      |
| vb\_1s\_andas                    | andas                     |
| vb\_2a\_ansöka                   | ansöka                    |
| vb\_2a\_göra                     | göra                      |
| vb\_2a\_hyra                     | hyra                      |
| vb\_2a\_känna                    | känna                     |
| vb\_2a\_leda                     | leda                      |
| vb\_2a\_lägga                    | lägga                     |
| vb\_2a\_sätta                    | sätta                     |
| vb\_2a\_viga                     | viga                      |
| vb\_2s\_synas                    | synas                     |
| vb\_4a\_falla                    | falla                     |
| vb\_4a\_flyga                    | flyga                     |
| vb\_4a\_ge                       | ge                        |
| vb\_4a\_hålla                    | hålla                     |
| vb\_4a\_komma                    | komma                     |
| vb\_4a\_rida                     | rida                      |
| vb\_4a\_skjuta                   | skjuta                    |
| vb\_4a\_tillåta                  | tillåta                   |
| vb\_4a\_slå                      | slå                       |
| vb\_4a\_se                       | se                        |
| vb\_4a\_gå                       | gå                        |
| vb\_4a\_dricka                   | dricka                    |
| vb\_4a\_bära                     | bära                      |
| vb\_4m\_innebära                 | bära                      |
| vb\_4a\_ta                       | ta                        |
| vb\_va\_klä                      | klä                       |
| vb\_4m\_angå                     | angå                      |
| vb\_4m\_stå                      | stå                       |
| vb\_4m\_vina                     | vina                      |
| vb\_va\_bringa                   | bringa                    |
| nn\_1u\_flicka                   | flicka                    |
| nn\_2u\_botten                   | botten                    |
| nn\_2u\_nyckel                   | nyckel                    |
| nn\_2u\_pojke                    | pojke                     |
| nn\_2u\_stol                     | stol                      |
| nn\_4u\_linje                    | linje                     |
| nn\_5n\_knä                      | knä                       |
| nn\_6n\_garage                   | garage                    |
| nn\_6n\_segel                    | segel                     |
| nn\_6u\_gås                      | gås                       |
| nn\_ou\_emeritus                 | emeritus                  |
| nn\_ou\_examen                   | examen                    |
| nn\_vn\_centrum                  | centrum                   |
| nn\_vn\_nomen                    | nomen                     |
| nn\_vu\_jojo                     | jojo                      |
| nn\_vu\_partner                  | partner                   |
| nn\_vv\_abdomen                  | abdomen                   |
| vb\_2a\_lyfta                    | lyfta                     |
| vb\_2a\_sända                    | sända                     |
| vb\_3a\_sy                       | sy                        |
| vb\_va\_koka                     | koka                      |
| vb\_va\_sprida                   | sprida                    |
| av\_2\_ung                       | ung                       |
| pm\_fph\_kleopatra               | kleopatra                 |
| pm\_uls\_storgatan               | storgatan                 |
| pm\_noe\_harvard                 | harvard                   |
| pm\_utz\_bambara                 | bambara                   |
| pm\_vlg\_nordsjön                | nordsjön                  |
| pm\_uoc\_operan                  | operan                    |
| pm\_ueh\_upplysningen            | upplysningen              |
| pm\_uaa\_viggen                  | viggen                    |
| pm\_naw\_titanic                 | titanic                   |
| pm\_upm\_audhumbla               | audhumbla                 |
| pm\_uog\_polisen                 | polisen                   |
| pm\_uaa\_camel                   | camel                     |
| pm\_poc\_hepstars                | hepstars                  |
| pm\_naa\_keso                    | keso                      |
| pm\_upa\_brunte                  | brunte                    |
| pm\_uop\_landsorganisationen     | landsorganisationen       |
| pm\_uoe\_kursverksamheten        | kursverksamheten          |
| pm\_uap\_vasaorden               | vasaorden                 |
| pm\_nwm\_aktuellt                | aktuellt                  |
| pm\_nop\_efta                    | efta                      |
| pm\_nog\_skatteverket            | skatteverket              |
| pm\_nog\_knesset                 | knesset                   |
| pm\_nog\_interpol                | interpol                  |
| pm\_noc\_musikforum              | musikforum                |
| pm\_nla\_solsystemet             | solsystemet               |
| pm\_hph\_af                      | af                        |
| pm\_fph\_barbro                  | barbro                    |
| pm\_uwa\_monalisa                | monalisa                  |
| pm\_upc\_ttaps-gruppen           | ttaps-gruppen             |
| pm\_uop\_atlantpakten            | atlantpakten              |
| pm\_uae\_keso                    | keso                      |
| pm\_nwp\_charta77                | charta77                  |
| pm\_nos\_gais                    | gais                      |
| pm\_nog\_efta                    | efta                      |
| pm\_noa\_finnair                 | finnair                   |
| pm\_nes\_vasaloppet              | vasaloppet                |
| pm\_nap\_nobelpriset             | nobelpriset               |
| pm\_naa\_camel                   | camel                     |
| pm\_fpm\_maria                   | maria                     |
| pma\_woc\_od                     | od                        |
| pma\_nwb\_blm                    | blm                       |
| pma\_nom\_svt                    | svt                       |
| pma\_nog\_ab                     | ab                        |
| pma\_noa\_sas                    | sas                       |
| pma\_nam\_thx                    | thx                       |
| pma\_naf\_jas                    | jas                       |
| pma\_naa\_lep                    | lep                       |
| pma\_mph\_jr                     | jr                        |
| pma\_hph\_nn                     | nn                        |
| pma\_noe\_gu                     | gu                        |
| pma\_nlp\_eu                     | eu                        |
| pma\_uwn\_dn                     | dn                        |
| pma\_ntm\_cp                     | cp                        |
| pma\_mpm\_st                     | st                        |
| pmm\_u0wc\_fröken\_julie          | fröken julie              |
| pmm\_u0la\_stora\_björnen         | stora björnen             |
| pmm\_n0op\_grön\_ungdom           | grön ungdom               |
| pmm\_u0tb\_betula\_alba           | betula alba               |
| pmm\_m0ph\_birger\_jarl           | birger jarl               |
| pmm\_u0oe\_svenska\_institutionen | svenska institutionen     |
| pmm\_u0aa\_koh\_i\_noor            | koh i noor                |
| pmm\_m0ph\_per\_olov              | per olov                  |
| pmm\_m0ph\_karl\_den\_tolfte       | karl den tolfte           |
| pmm\_m0ph\_el\_greco              | el greco                  |
| pmm\_f0pm\_jungfru\_maria         | jungfru maria             |
| pmm\_n0lf\_vita\_huset            | vita huset                |
| pmm\_m0ph\_karl\_xii              | karl xii                  |
| pmm\_h0ph\_jonsson\_lind          | jonsson lind              |
| pmm\_u0tm\_parkinsons\_sjukdom    | parkinsons sjukdom        |
| pmm\_u0op\_nysvenska\_rörelsen    | nysvenska rörelsen        |
| pmm\_u0ls\_lilla\_nygatan         | lilla nygatan             |
| pmm\_u0en\_big\_bang              | big bang                  |
| pmm\_u0aw\_cutty\_sark            | cutty sark                |
| pmm\_n0oc\_ebba\_grön             | ebba grön                 |
| pmm\_m0pm\_john\_blund            | john blund                |
| pmm\_m0ph\_adam\_av\_bremen        | adam av bremen            |
| pmm\_v0lf\_notre\_dame            | notre dame                |
| pmm\_u0wn\_dagens\_nyheter        | dagens nyheter            |
| pmm\_u0es\_davis\_cup             | davis cup                 |
| pmm\_u0er\_marie\_bebådelse       | marie bebådelse           |
| pmm\_u0eh\_franska\_revolutionen  | franska revolutionen      |
| pmm\_u0ag\_rolls\_royce           | rolls royce               |
| pmm\_p0ph\_bröderna\_grim         | bröderna grim             |
| pmm\_m0ph\_plinius\_d\_y           | plinius d y               |
| pmm\_m0pa\_pelle\_svanslös        | pelle svanslös            |
| pmm\_f0ph\_eva\_ek                | eva ek                    |
| pmm\_uatm\_multipel\_skleros      | multipel skleros          |
| pmm\_uatm\_cerebral\_pares        | cerebral pares            |
| pmm\_pcpm\_hugin\_och\_munin       | hugin och munin           |
| pmm\_f1pm\_jungfrun\_från\_orleans | jungfrun från orleans     |
| pmm\_nu0wn\_svenska\_dagbladet    | svenska dagbladet         |
| ab\_2\_bra                       | bra                       |
| ab\_2\_nära                      | nära                      |
| ab\_2\_mycket                    | mycket                    |
| ab\_2\_länge                     | länge                     |
| ab\_2\_illa                      | illa                      |
| ab\_2\_gärna                     | gärna                     |
| ab\_2\_föga                      | föga                      |
| ab\_ik\_vidare                   | vidare                    |
| ab\_2\_lite                      | lite                      |
| ie\_i\_att                       | att                       |
| sn\_i\_om                        | om                        |
| nn\_3n\_parti                    | parti                     |
| nn\_3u\_fiber                    | fiber                     |
| nn\_3u\_tand                     | tand                      |
| nn\_3u\_film                     | film                      |
| nn\_3u\_akademi                  | akademi                   |
| nn\_dn\_rubbet                   | rubbet                    |
| nn\_dp\_tropikerna               | tropikerna                |
| nn\_du\_stampen                  | stampen                   |
| nn\_np\_ordalag                  | ordalag                   |
| nn\_rp\_benvärmare               | benvärmare                |
| nn\_rp\_griller                  | griller                   |
| pn\_o\_sån                       | sån                       |
| pn\_o\_varsin                    | varsin                    |
| pn\_o\_vem                       | vem                       |
| pn\_o\_sig                       | sig                       |
| pn\_o\_ingen                     | ingen                     |
| pn\_o\_den                       | den                       |
| pn\_o\_någon                     | någon                     |
| pn\_o\_ingendera                 | ingendera                 |
| pn\_o\_vi                        | vi                        |
| pn\_o\_de                        | de                        |
| pn\_o\_varenda                   | varenda                   |
| pn\_o\_vardera                   | vardera                   |
| pn\_o\_varannan                  | varannan                  |
| pn\_o\_var                       | var                       |
| pn\_o\_samma                     | samma                     |
| pn\_o\_ni                        | ni                        |
| pn\_o\_mången                    | mången                    |
| pn\_o\_jag                       | jag                       |
| pn\_o\_högstdensamme             | högstdensamme             |
| pn\_o\_hon                       | hon                       |
| pn\_o\_han                       | han                       |
| pn\_o\_endera                    | endera                    |
| pn\_o\_ena                       | ena                       |
| pn\_o\_du                        | du                        |
| pn\_o\_densamma                  | densamma                  |
| pn\_o\_denna                     | denna                     |
| pn\_o\_annan                     | annan                     |
| al\_o\_den                       | den                       |
| nn\_vu\_mixer                    | mixer                     |
| nn\_vn\_medium                   | medium                    |
| nn\_on\_memorandum               | memorandum                |
| nn\_1u\_åder                     | åder                      |
| nn\_vu\_kart                     | kart                      |
| nn\_vn\_mirakel                  | mirakel                   |
| nn\_0v\_trim                     | trim                      |
| nn\_0v\_tö                       | tö                        |
| nn\_0n\_bitumen                  | bitumen                   |
| nn\_vn\_lexikon                  | lexikon                   |
| nn\_2u\_slarver                  | slarver                   |
| nn\_2u\_bräken                   | bräken                    |
| nn\_2u\_socken                   | socken                    |
| nn\_2u\_himmel                   | himmel                    |
| nn\_4n\_fängelse                 | fängelse                  |
| nn\_2u\_dag                      | dag                       |
| nn\_vu\_ponny                    | ponny                     |
| nn\_vu\_kollega                  | kollega                   |
| nn\_vn\_kolli                    | kolli                     |
| nn\_6u\_tum                      | tum                       |
| nn\_6n\_universum                | universum                 |
| nn\_0n\_gluten                   | gluten                    |
| nn\_vu\_yard                     | yard                      |
| nn\_vu\_svan                     | svan                      |
| nn\_vn\_tema                     | tema                      |
| nn\_vn\_perfektum                | perfektum                 |
| nn\_vn\_maximum                  | maximum                   |
| nn\_vn\_frö                      | frö                       |
| nn\_3u\_materia                  | materia                   |
| nn\_0n\_delirium                 | delirium                  |
| nn\_vv\_fossil                   | fossil                    |
| nn\_vv\_libretto                 | libretto                  |
| nn\_vu\_safari                   | safari                    |
| nn\_vu\_bungalow                 | bungalow                  |
| nn\_ip\_honoratiores             | honoratiores              |
| nn\_7u\_lady                     | lady                      |
| nn\_6v\_kvitten                  | kvitten                   |
| nn\_vv\_franska                  | franska                   |
| nn\_4v\_folie                    | folie                     |
| nn\_3u\_donjuan                  | donjuan                   |
| nn\_2v\_finger                   | finger                    |
| nn\_2u\_biceps                   | biceps                    |
| nn\_1u\_ultima                   | ultima                    |
| nn\_0n\_opium                    | opium                     |
| nn\_vv\_skogsrå                  | skogsrå                   |
| nn\_vv\_prisma                   | prisma                    |
| nn\_vv\_hult                     | hult                      |
| nn\_vu\_spaniel                  | spaniel                   |
| nn\_vu\_litteraturkanon          | litteraturkanon           |
| nn\_vu\_igloo                    | igloo                     |
| nn\_vn\_alfa\_io                  | io                        |
| nn\_6n\_deponens                 | deponens                  |
| nn\_6n\_andeväsen                | andeväsen                 |
| nn\_5n\_altare                   | altare                    |
| nn\_3u\_geranium                 | geranium                  |
| nn\_2u\_timma                    | timma                     |
| nn\_1u\_toffel                   | toffel                    |
| nn\_0v\_dregel                   | dregel                    |
| nn\_0u\_koppar                   | koppar                    |
| nn\_vv\_paraply                  | paraply                   |
| nn\_vv\_etage                    | etage                     |
| nn\_vv\_chiffer                  | chiffer                   |
| nn\_vv\_bolster                  | bolster                   |
| nn\_vu\_teve                     | teve                      |
| nn\_vu\_rhododendron             | rhododendron              |
| nn\_vu\_kofot                    | kofot                     |
| nn\_vu\_jourhavande              | jourhavande               |
| nn\_vu\_jockey                   | jockey                    |
| nn\_vu\_grej                     | grej                      |
| nn\_vu\_drive                    | drive                     |
| nn\_vu\_cello                    | cello                     |
| nn\_vn\_trauma                   | trauma                    |
| nn\_vn\_stall                    | stall                     |
| nn\_vn\_pi                       | pi                        |
| nn\_vn\_paper                    | paper                     |
| nn\_vn\_panorama                 | panorama                  |
| nn\_vn\_logi                     | logi                      |
| nn\_ou\_medikus                  | medikus                   |
| nn\_6v\_årder                    | årder                     |
| nn\_6v\_hästskosöm               | hästskosöm                |
| nn\_6u\_morbroder                | morbror                   |
| nn\_6n\_pansar                   | pansar                    |
| nn\_3n\_center                   | center                    |
| nn\_6n\_ankare                   | ankare                    |
| nn\_3v\_plasma                   | plasma                    |
| nn\_3u\_papyrus                  | papyrus                   |
| nn\_3n\_alluvium                 | alluvium                  |
| nn\_3n\_alkali                   | alkali                    |
| nn\_2v\_skit                     | skit                      |
| nn\_2u\_toddy                    | toddy                     |
| nn\_0u\_praxis                   | praxis                    |
| nn\_vv\_stimulus                 | stimulus                  |
| nn\_vv\_rå\_gång                  | rå                        |
| nn\_vv\_ringfinger               | ringfinger                |
| nn\_vv\_ordal                    | ordal                     |
| nn\_vv\_halvankare               | halvankare                |
| nn\_vu\_western                  | western                   |
| nn\_vu\_torso                    | torso                     |
| nn\_vu\_spång                    | spång                     |
| nn\_vu\_spann                    | spann                     |
| nn\_vu\_scarf                    | scarf                     |
| nn\_vu\_rubel                    | rubel                     |
| nn\_vu\_ro                       | ro                        |
| nn\_vu\_promovend                | promovend                 |
| nn\_vu\_preses                   | preses                    |
| nn\_vu\_paria                    | paria                     |
| nn\_vu\_mikron                   | mikron                    |
| nn\_vu\_lama                     | lama                      |
| nn\_vu\_glass                    | glass                     |
| nn\_vu\_gladiolus                | gladiolus                 |
| nn\_vu\_baby                     | baby                      |
| nn\_vu\_albino                   | albino                    |
| nn\_vn\_stånd                    | stånd                     |
| nn\_vn\_solo                     | solo                      |
| nn\_vn\_serum                    | serum                     |
| nn\_vn\_scenario                 | scenario                  |
| nn\_vn\_rö                       | rö                        |
| nn\_vn\_rekviem                  | rekviem                   |
| nn\_vn\_omen                     | omen                      |
| nn\_vn\_mineral                  | mineral                   |
| nn\_vn\_lim                      | lim                       |
| nn\_vn\_kompositum               | kompositum                |
| nn\_vn\_ja                       | ja                        |
| nn\_vn\_härad                    | härad                     |
| nn\_vn\_gag                      | gag                       |
| nn\_vn\_gage                     | gage                      |
| nn\_vn\_apropå                   | apropå                    |
| nn\_vn\_alfa\_z                   | z                         |
| nn\_vn\_ackordion                | ackordion                 |
| nn\_ov\_styck                    | styck                     |
| nn\_ov\_diktamen                 | diktamen                  |
| nn\_ou\_putto                    | putto                     |
| nn\_ou\_penny                    | penny                     |
| nn\_ou\_mekanikus                | mekanikus                 |
| nn\_on\_slusshuvud               | slusshuvud                |
| nn\_on\_gravamen                 | gravamen                  |
| nn\_7u\_slogan                   | slogan                    |
| nn\_7n\_skinhead                 | skinhead                  |
| nn\_6v\_modus                    | modus                     |
| nn\_6v\_data                     | data                      |
| nn\_6u\_man                      | man                       |
| nn\_6u\_iktus                    | iktus                     |
| nn\_6n\_interregnum              | interregnum               |
| nn\_5n\_ri                       | ri                        |
| nn\_3v\_gelé                     | gelé                      |
| nn\_3u\_fotnot                   | fotnot                    |
| nn\_3u\_farao                    | farao                     |
| nn\_3u\_eforus                   | eforus                    |
| nn\_3n\_seminarium               | seminarium                |
| nn\_3n\_futurum                  | futurum                   |
| nn\_3n\_dominion                 | dominion                  |
| nn\_3v\_aktivum                  | aktivum                   |
| nn\_2u\_stygger                  | stygger                   |
| nn\_2u\_förmiddag                | förmiddag                 |
| nn\_2u\_andur                    | andur                     |
| nn\_1v\_antibiotika              | antibiotika               |
| nn\_0v\_status                   | status                    |
| nn\_0v\_hysteri                  | hysteri                   |
| nn\_0v\_facit                    | facit                     |
| nn\_0u\_makadam                  | makadam                   |
| nn\_0u\_aorta                    | aorta                     |
| nn\_0n\_karborundum              | karborundum               |
| nn\_0n\_kammarkollegium          | kammarkollegium           |
| nn\_0n\_gehenna                  | gehenna                   |
| nn\_2u\_bövel                    | bövel                     |
| av\_0s\_minst                    | minst                     |
| av\_0\_korkad                    | korkad                    |
| av\_1\_enkel                     | enkel                     |
| av\_in\_lurt                     | lurt                      |
| av\_id\_norra                    | norra                     |
| av\_im\_bemälde                  | bemälde                   |
| av\_ik\_smärre                   | smärre                    |
| avm\_ix0\_diverse                | diverse                   |
| avm\_io0\_diverse                | diverse                   |
| avm\_ia0\_diverse                | diverse                   |
| avm\_ip0\_diverse                | diverse                   |
| av\_0\_skriftlärd                | skriftlärd                |
| vb\_1s\_gillas                   | gillas                    |
| vb\_1a\_spara                    | spara                     |
| vb\_1a\_skapa                    | skapa                     |
| vb\_1a\_ugnsbaka                 | ugnsbaka                  |
| vb\_1m\_vånna                    | vånna                     |
| vb\_1m\_kackla                   | kackla                    |
| vb\_1a\_unna                     | unna                      |
| vb\_1a\_häda                     | häda                      |
| vb\_4m\_ljuda                    | ljuda                     |
| vb\_4a\_fara                     | fara                      |
| vb\_2a\_leva                     | leva                      |
| vb\_2a\_stödja                   | stödja                    |
| vb\_2a\_sälja                    | sälja                     |
| vb\_2a\_säga                     | säga                      |
| vb\_2a\_motsäga                  | motsäga                   |
| vb\_2a\_mista                    | mista                     |
| vb\_2a\_välja                    | välja                     |
| vb\_2m\_hända                    | hända                     |
| vb\_2d\_må                       | må                        |
| vb\_2m\_gitta                    | gitta                     |
| vb\_2a\_städja                   | städja                    |
| vb\_2a\_genmäla                  | genmäla                   |
| vb\_2m\_väga                     | väga                      |
| vb\_2m\_höta                     | höta                      |
| vb\_2m\_glädja                   | glädja                    |
| vb\_2m\_böra                     | böra                      |
| vb\_2a\_tämja                    | tämja                     |
| vb\_2a\_spörja                   | spörja                    |
| vb\_2s\_blygas                   | blygas                    |
| vb\_2s\_trivas                   | trivas                    |
| vb\_2s\_nöjas                    | nöjas                     |
| vb\_2s\_minnas                   | minnas                    |
| vb\_2s\_vämjas                   | vämjas                    |
| vb\_2s\_töras                    | töras                     |
| vb\_2s\_rymmas                   | rymmas                    |
| vb\_2s\_idas                     | idas                      |
| vb\_2s\_hövas                    | hövas                     |
| vb\_2s\_glädjas                  | glädjas                   |
| vb\_2s\_giftas                   | giftas                    |
| vb\_2s\_skiljas                  | skiljas                   |
| vb\_va\_vika                     | vika                      |
| vb\_va\_tvinga                   | tvinga                    |
| vb\_va\_löpa                     | löpa                      |
| vb\_vm\_fnysa                    | fnysa                     |
| vb\_vm\_avvara                   | avvara                    |
| vb\_va\_växa                     | växa                      |
| vb\_va\_stupa                    | stupa                     |
| vb\_va\_lyda                     | lyda                      |
| vb\_om\_skola                    | skola                     |
| vb\_vm\_snika                    | snika                     |
| vb\_vm\_smälla                   | smälla                    |
| vb\_va\_tälja                    | tälja                     |
| vb\_va\_två                      | två                       |
| vb\_va\_smälta                   | smälta                    |
| vb\_va\_förmäla                  | förmäla                   |
| vb\_va\_besvärja                 | besvärja                  |
| vb\_om\_kunna                    | kunna                     |
| vb\_vm\_upphäva                  | upphäva                   |
| vb\_vm\_undvara                  | undvara                   |
| vb\_vm\_strida                   | strida                    |
| vb\_vm\_sluta                    | sluta                     |
| vb\_vm\_samvara                  | samvara                   |
| vb\_vm\_ryka                     | ryka                      |
| vb\_vm\_nysa                     | nysa                      |
| vb\_vm\_kvida                    | kvida                     |
| vb\_vm\_klinga                   | klinga                    |
| vb\_vm\_gälla\_kastrera           | gälla                     |
| vb\_vm\_gala                     | gala                      |
| vb\_vm\_duga                     | duga                      |
| vb\_vm\_drösa                    | drösa                     |
| vb\_vm\_drypa                    | drypa                     |
| vb\_va\_utlöpa                   | utlöpa                    |
| vb\_va\_träda                    | träda                     |
| vb\_va\_strypa                   | strypa                    |
| vb\_va\_snusmala                 | snusmala                  |
| vb\_va\_skvätta                  | skvätta                   |
| vb\_va\_simma                    | simma                     |
| vb\_va\_nästa                    | nästa                     |
| vb\_va\_mala                     | mala                      |
| vb\_va\_kväda                    | kväda                     |
| vb\_va\_klyva                    | klyva                     |
| vb\_va\_gälda                    | gälda                     |
| vb\_va\_förse                    | förse                     |
| vb\_va\_förlöpa                  | förlöpa                   |
| vb\_va\_framtvinga               | framtvinga                |
| vb\_va\_tala                     | tala                      |
| vb\_va\_bestrida                 | bestrida                  |
| vb\_va\_besluta                  | besluta                   |
| vb\_va\_begrava                  | begrava                   |
| vb\_om\_vilja                    | vilja                     |
| vb\_om\_veta                     | veta                      |
| vb\_om\_måste                    | måste                     |
| vb\_om\_heta                     | heta                      |
| vb\_oa\_varda                    | varda                     |
| vb\_vs\_nypas                    | nypas                     |
| vb\_vs\_dväljas                  | dväljas                   |
| vb\_4s\_vederfaras               | vederfaras                |
| vb\_4s\_umgås                    | umgås                     |
| vb\_4s\_munhuggas                | munhuggas                 |
| vb\_4s\_bitas                    | bitas                     |
| vb\_4s\_hållas                   | hållas                    |
| vb\_4s\_finnas                   | finnas                    |
| vb\_4s\_stickas                  | stickas                   |
| vb\_4s\_slåss                    | slåss                     |
| vb\_4s\_ses                      | ses                       |
| vb\_4s\_ges                      | givas                     |
| vb\_4m\_svälta\_1                 | svälta                    |
| vb\_va\_svälta\_2                 | svälta                    |
| vb\_4m\_förslå                   | förslå                    |
| vb\_4a\_stjäla                   | stjäla                    |
| vb\_4m\_vara                     | vara                      |
| vb\_4m\_sova                     | sova                      |
| vb\_4m\_erfara                   | erfara                    |
| vb\_4a\_bli                      | bli                       |
| vb\_4a\_bestå                    | bestå                     |
| vb\_4a\_äta                      | äta                       |
| vb\_4a\_svära                    | svära                     |
| vb\_4a\_emotstå                  | emotstå                   |
| vb\_4m\_sitta                    | sitta                     |
| vb\_4a\_be                       | be                        |
| vb\_4m\_ryta                     | ryta                      |
| vb\_4m\_gråta                    | gråta                     |
| vb\_4m\_ligga                    | ligga                     |
| vb\_4m\_le                       | le                        |
| vb\_4m\_bekomma                  | bekomma                   |
| vb\_4m\_småsvära                 | småsvära                  |
| vb\_4m\_skåpäta                  | skåpäta                   |
| vb\_4m\_förevara                 | förevara                  |
| vb\_4a\_stinga                   | stinga                    |
| vb\_4a\_förgäta                  | förgäta                   |
| vb\_3a\_trä                      | trä                       |
| vb\_2m\_ha                       | ha                        |
| av\_0\_höger                     | höger                     |
| av\_0\_fullmäktig                | fullmäktig                |
| av\_0\_världsbäst                | världsbäst                |
| av\_0\_vareviga                  | vareviga                  |
| av\_0\_sankt                     | sankt                     |
| av\_0\_pytteliten                | pytteliten                |
| av\_0\_blott                     | blott                     |
| av\_0s\_innersta                 | innersta                  |
| av\_0s\_självaste                | självaste                 |
| av\_0p\_samtliga                 | samtliga                  |
| av\_0d\_nästa                    | nästa                     |
| av\_0d\_enda                     | enda                      |
| av\_v\_ond                       | ond                       |
| av\_v\_god                       | god                       |
| av\_2\_gammal                    | gammal                    |
| av\_2\_bra                       | bra                       |
| av\_v\_nära                      | nära                      |
| av\_v\_förnäm                    | förnäm                    |
| av\_v\_dålig                     | dålig                     |
| av\_2k\_bakre                    | bakre                     |
| av\_2\_mycken                    | mycken                    |
| av\_2\_liten                     | liten                     |
| av\_2\_få                        | få                        |
| av\_1\_orange                    | orange                    |
| av\_1\_knall                     | knall                     |
| av\_1\_camp                      | camp                      |
| av\_1\_beige                     | beige                     |
| av\_1\_ball                      | ball                      |
| nnm\_vv0\_libretto               | allt i allo               |
| nnm\_0v\_facit                   | modus vivendi             |
| nnm\_0v\_manna                   | lingua franca             |
| nnm\_du0\_stampen                | gordiska knuten           |
| nnm\_vu0\_mikron                 | grand danois              |
| nnm\_vu0\_trio                   | femme fatale              |
| nnm\_vu0\_bungalow               | spin off                  |
| nnm\_vv0\_pain\_riche             | pain riche                |
| nnm\_vv0\_deja\_vu                | déjá vu                   |
| nnm\_rp0\_griller                | scampi fritti             |
| nnm\_vn0\_alfa\_z                 | ettstrukna c              |
| nnm\_su0\_pojke                  | dödens lammunge           |
| nnm\_su0\_tro                    | janssons frestelse        |
| nnm\_7u0\_hit                    | negro spiritual           |
| nnm\_7n0\_skinhead               | practical joke            |
| nnm\_6u0\_yen                    | pol mag                   |
| nnm\_6n0\_blad                   | flygande tefat            |
| nnm\_5n0\_ansikte                | da capo                   |
| nnm\_ip0\_honoratiores           | lika goda kålsupare       |
| nnm\_3u0\_film                   | medicine kandidat         |
| nnm\_2u0\_stol                   | vinst- och förlusträkning |
| nnm\_2u0\_nyckel                 | golden retriever          |
| nnm\_rp1\_vägnar                 | fjärilar i magen          |
| nnm\_1u0\_flicka                 | mul- och klövsjuka        |
| nnm\_0u0\_hin                    | hin håle                  |
| nnm\_0u0\_frid                   | rhode islandsås           |
| nnm\_np0\_ordalag                | nordiska språk            |
| nnm\_4u0\_linje                  | eau de cologne            |
| nnm\_2u1\_stol                   | ulv i fårakläder          |
| nnm\_2u0\_pojke                  | vandrande pinne           |
| nnm\_rp0\_vägnar                 | reda pengar               |
| nnm\_rp0\_kårar                  | kalla kårar               |
| nnm\_1u1\_flicka                 | fnurra på tråden          |
| nnm\_0u0\_tro                    | cherry brandy             |
| nnm\_0u0\_antimateria            | idé- och lärdomshistoria  |
| nnm\_in0\_vaj                    | berått mod                |
| nnm\_iu0\_vift                   | gilla gång                |
| nnm\_dn0\_rubbet                 | rubbet                    |
| nna\_0v\_pcb                     | pcb                       |
| nna\_0n\_hk                      | hk                        |
| nna\_0u\_jo                      | jo                        |
| nna\_vv\_dna                     | dna                       |
| nna\_6n\_kg                      | kg                        |
| nna\_6u\_lp                      | lp                        |
| nna\_in\_ex                      | ex                        |
| nna\_6n\_ekg                     | ekg                       |
| nna\_vn\_wc                      | wc                        |
| nna\_6v\_pm                      | pm                        |
| nna\_2u\_bh                      | bh                        |
| avm\_0p0\_gul                    | rangen stridig            |
| avm\_0a0\_diverse                | ute efter                 |
| avm\_0x0\_utbrunnen              | inte oäven                |
| avm\_0x0\_korkad                 | fly förbannad             |
| avm\_0a0\_korkad                 | så kallad                 |
| avm\_0x0\_gul                    | inte så pjåkig            |
| avm\_0x0\_bred                   | naggande god              |
| avm\_0p1\_rund                   | inställd på               |
| avm\_0p1\_gul                    | hal som en ål             |
| avm\_0p1\_brydd                  | stadd i                   |
| avm\_0p0\_diverse                | ute efter                 |
| avm\_1p1\_bred                   | glad i                    |
| avm\_1p1\_akut                   | fäst vid                  |
| al\_o\_en                        | en                        |
| nn\_0n\_hindi                    | hindi                     |
| nn\_0n\_kol-14                   | kol-14                    |
| nn\_0u\_hin                      | hin                       |
| av\_0\_deputerad                 | deputerad                 |
| nn\_6u\_yen                      | yen                       |
| nn\_vu\_bagis                    | bagis                     |
| nn\_vu\_order                    | order                     |
| nn\_vu\_minut                    | minut                     |
| av\_0\_uppsutten                 | uppsutten                 |
| av\_0\_uppvikt                   | uppvikt                   |
| av\_v\_trång                     | trång                     |
| pnm\_x1\_vad\_än                  | vad än                    |
| pnm\_i\_ditt\_och\_datt            | ditt och datt             |
| pnm\_o\_vem\_som\_helst            | vem som helst             |
| vb\_2m\_mysa                     | mysa                      |
| vb\_va\_nypa                     | nypa                      |
| vb\_0d\_lyster                   | lyster                    |
| vb\_0d\_värdes                   | värdes                    |
| vb\_0d\_vederböra                | vederböra                 |
| vb\_0d\_nåde                     | nåde                      |
| vb\_0d\_lyss                     | lyss                      |
| vb\_4d\_vederfås                 | vederfås                  |
| vb\_4d\_sprätta                  | spritta                   |
| vb\_id\_månde                    | månde                     |
| vb\_2d\_torde                    | torde                     |
| vb\_2d\_rädas                    | rädas                     |
| nnm\_6na\_segel                  | kort varsel               |
| nnm\_6ua\_gås                    | oplockad gås              |
| nnm\_0ua\_frid                   | fri lejd                  |
| nnm\_3ua\_film                   | fransysk visit            |
| nnm\_3ua\_enarmad\_bandit         | enarmad bandit            |
| nnm\_2ua\_pojke                  | finsk pinne               |
| nnm\_dpc\_göranden\_och\_låtanden  | göranden och låtanden     |
| nnm\_0n0\_hindi                  | perpetuum mobile          |
| nnm\_0na\_syre                   | fritt vivre               |
| nnm\_2ua\_stol                   | varm korv                 |
| nnm\_npc\_kreti\_och\_pleti        | kreti och pleti           |
| ava\_i\_kungl                    | Kungl.                    |
| vba\_ia\_jfr                     | jfr                       |
| ppa\_i\_pga                      | pga                       |
| ppm\_i\_a\_la                     | a la                      |
| kna\_i\_o                        | o                         |
| snm\_i\_efter\_det\_att            | efter det att             |
| knm\_x\_ju\_ju                    | ju ju                     |
| knm\_i\_vare\_sig                 | ju ju                     |
| abm\_x1\_var\_än                  | var än                    |
| ppm\_x1\_för\_skull               | för skull                 |
| ssm\_d2\_svinhugg\_går\_igen       | svinhugg går igen         |
| ssm\_i1\_märk\_väl                | märk väl                  |
| ssm\_d2\_saken\_är\_biff           | saken är biff             |
| nlm\_gi\_tusen\_sinom\_tusen       | tusen sinom tusen         |

## License

The original Saldo is dual-licensed under CC-BY and LGPL v.3. As a result, `saldo-inflector` is under the LGPL v.3 license, copyright 2023 Kielikone oy.
