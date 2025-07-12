{ UTF-8 to HTML conversion map }

{ Created by CpME's map creator v2025-07-09 }
{ https://github.com/shidel/CpME/           }


{$IFNDEF TP70}

{$IFNDEF TextRemapEntries}
{$DEFINE TextRemapEntries}
type
  TTextRemapEntry = record
    Value : AnsiString;
    Data : AnsiString;
  end;
  TTextRemapEntries = array of TTextRemapEntry;
{$ENDIF}

{$DEFINE UTF8toHTMLRemap}
const
  UTF8toHTMLRemapList : TTextRemapEntries = (
    (Value:'306';           Data:'IJlig'),
    (Value:'10920';         Data:'lescc'),
    (Value:'10670';         Data:'angmsdag'),
    (Value:'10553';         Data:'larrpl'),
    (Value:'1044';          Data:'Dcy'),
    (Value:'10231';         Data:'xharr'),
    (Value:'1014';          Data:'bepsi'),
    (Value:'10072';         Data:'VerticalSeparator'),
    (Value:'10007';         Data:'cross'),
    (Value:'10003';         Data:'check'),
    (Value:'10016';         Data:'malt'),
    (Value:'10038';         Data:'sext'),
    (Value:'10098';         Data:'lbbrk'),
    (Value:'1008';          Data:'kappav'),
    (Value:'1009';          Data:'rhov'),
    (Value:'10099';         Data:'rbbrk'),
    (Value:'1013';          Data:'epsiv'),
    (Value:'10218';         Data:'Lang'),
    (Value:'102,106';       Data:'fjlig'),
    (Value:'10184';         Data:'bsolhsub'),
    (Value:'10185';         Data:'suphsol'),
    (Value:'10214';         Data:'lobrk'),
    (Value:'10215';         Data:'robrk'),
    (Value:'10221';         Data:'roang'),
    (Value:'10219';         Data:'Rang'),
    (Value:'10220';         Data:'loang'),
    (Value:'10229';         Data:'xlarr'),
    (Value:'10230';         Data:'xrarr'),
    (Value:'1031';          Data:'YIcy'),
    (Value:'1025';          Data:'IOcy'),
    (Value:'10234';         Data:'xhArr'),
    (Value:'10232';         Data:'xlArr'),
    (Value:'10233';         Data:'xrArr'),
    (Value:'10236';         Data:'xmap'),
    (Value:'10239';         Data:'dzigrarr'),
    (Value:'1028';          Data:'Jukcy'),
    (Value:'1026';          Data:'DJcy'),
    (Value:'1027';          Data:'GJcy'),
    (Value:'1029';          Data:'DScy'),
    (Value:'1030';          Data:'Iukcy'),
    (Value:'1038';          Data:'Ubrcy'),
    (Value:'1034';          Data:'NJcy'),
    (Value:'1032';          Data:'Jsercy'),
    (Value:'1033';          Data:'LJcy'),
    (Value:'1035';          Data:'TSHcy'),
    (Value:'1036';          Data:'KJcy'),
    (Value:'1041';          Data:'Bcy'),
    (Value:'1039';          Data:'DZcy'),
    (Value:'1040';          Data:'Acy'),
    (Value:'1042';          Data:'Vcy'),
    (Value:'1043';          Data:'Gcy'),
    (Value:'10522';         Data:'ratail'),
    (Value:'10508';         Data:'lbarr'),
    (Value:'1049';          Data:'Jcy'),
    (Value:'1046';          Data:'ZHcy'),
    (Value:'1045';          Data:'IEcy'),
    (Value:'1047';          Data:'Zcy'),
    (Value:'1048';          Data:'Icy'),
    (Value:'1050';          Data:'Kcy'),
    (Value:'10498';         Data:'nvlArr'),
    (Value:'10499';         Data:'nvrArr'),
    (Value:'10500';         Data:'nvHarr'),
    (Value:'10501';         Data:'Map'),
    (Value:'10513';         Data:'DDotrahd'),
    (Value:'10510';         Data:'lBarr'),
    (Value:'10509';         Data:'rbarr'),
    (Value:'1051';          Data:'Lcy'),
    (Value:'10511';         Data:'rBarr'),
    (Value:'10512';         Data:'RBarr'),
    (Value:'10518';         Data:'Rarrtl'),
    (Value:'10514';         Data:'UpArrowBar'),
    (Value:'10515';         Data:'DownArrowBar'),
    (Value:'1052';          Data:'Mcy'),
    (Value:'10521';         Data:'latail'),
    (Value:'10535';         Data:'nwnear'),
    (Value:'10528';         Data:'rarrbfs'),
    (Value:'10525';         Data:'larrfs'),
    (Value:'10523';         Data:'lAtail'),
    (Value:'10524';         Data:'rAtail'),
    (Value:'10526';         Data:'rarrfs'),
    (Value:'10527';         Data:'larrbfs'),
    (Value:'10532';         Data:'nearhk'),
    (Value:'1053';          Data:'Ncy'),
    (Value:'10531';         Data:'nwarhk'),
    (Value:'10533';         Data:'searhk'),
    (Value:'10534';         Data:'swarhk'),
    (Value:'10547';         Data:'rarrc'),
    (Value:'10538';         Data:'swnwar'),
    (Value:'10536';         Data:'toea'),
    (Value:'10537';         Data:'tosa'),
    (Value:'1054';          Data:'Ocy'),
    (Value:'10547,824';     Data:'nrarrc'),
    (Value:'10550';         Data:'ldca'),
    (Value:'10549';         Data:'cudarrr'),
    (Value:'1055';          Data:'Pcy'),
    (Value:'10551';         Data:'rdca'),
    (Value:'10552';         Data:'cudarrl'),
    (Value:'10608';         Data:'RoundImplies'),
    (Value:'10586';         Data:'LeftTeeVector'),
    (Value:'10575';         Data:'RightUpDownVector'),
    (Value:'10568';         Data:'harrcir'),
    (Value:'10557';         Data:'cularrp'),
    (Value:'10556';         Data:'curarrm'),
    (Value:'1056';          Data:'Rcy'),
    (Value:'10565';         Data:'rarrpl'),
    (Value:'10570';         Data:'lurdshar'),
    (Value:'10569';         Data:'Uarrocir'),
    (Value:'1057';          Data:'Scy'),
    (Value:'10571';         Data:'ldrushar'),
    (Value:'10574';         Data:'LeftRightVector'),
    (Value:'10580';         Data:'RightUpVectorBar'),
    (Value:'10578';         Data:'LeftVectorBar'),
    (Value:'10576';         Data:'DownLeftRightVector'),
    (Value:'10577';         Data:'LeftUpDownVector'),
    (Value:'10579';         Data:'RightVectorBar'),
    (Value:'1058';          Data:'Tcy'),
    (Value:'10583';         Data:'DownRightVectorBar'),
    (Value:'10581';         Data:'RightDownVectorBar'),
    (Value:'10582';         Data:'DownLeftVectorBar'),
    (Value:'10584';         Data:'LeftUpVectorBar'),
    (Value:'10585';         Data:'LeftDownVectorBar'),
    (Value:'10597';         Data:'dHar'),
    (Value:'10591';         Data:'DownRightTeeVector'),
    (Value:'10589';         Data:'RightDownTeeVector'),
    (Value:'10587';         Data:'RightTeeVector'),
    (Value:'10588';         Data:'RightUpTeeVector'),
    (Value:'1059';          Data:'Ucy'),
    (Value:'10590';         Data:'DownLeftTeeVector'),
    (Value:'10594';         Data:'lHar'),
    (Value:'10592';         Data:'LeftUpTeeVector'),
    (Value:'10593';         Data:'LeftDownTeeVector'),
    (Value:'10595';         Data:'uHar'),
    (Value:'10596';         Data:'rHar'),
    (Value:'10602';         Data:'lharul'),
    (Value:'1060';          Data:'Fcy'),
    (Value:'10598';         Data:'luruhar'),
    (Value:'10599';         Data:'ldrdhar'),
    (Value:'10600';         Data:'ruluhar'),
    (Value:'10601';         Data:'rdldhar'),
    (Value:'10605';         Data:'lrhard'),
    (Value:'10603';         Data:'llhard'),
    (Value:'10604';         Data:'rharul'),
    (Value:'10606';         Data:'udhar'),
    (Value:'10607';         Data:'duhar'),
    (Value:'1064';          Data:'SHcy'),
    (Value:'10620';         Data:'lfisht'),
    (Value:'10613';         Data:'rarrap'),
    (Value:'10610';         Data:'simrarr'),
    (Value:'10609';         Data:'erarr'),
    (Value:'1061';          Data:'KHcy'),
    (Value:'10611';         Data:'larrsim'),
    (Value:'10612';         Data:'rarrsim'),
    (Value:'10617';         Data:'subrarr'),
    (Value:'10614';         Data:'ltlarr'),
    (Value:'10616';         Data:'gtrarr'),
    (Value:'10619';         Data:'suplarr'),
    (Value:'1062';          Data:'TScy'),
    (Value:'10630';         Data:'ropar'),
    (Value:'10623';         Data:'dfisht'),
    (Value:'10621';         Data:'rfisht'),
    (Value:'10622';         Data:'ufisht'),
    (Value:'10629';         Data:'lopar'),
    (Value:'1063';          Data:'CHcy'),
    (Value:'10637';         Data:'lbrkslu'),
    (Value:'10635';         Data:'lbrke'),
    (Value:'10636';         Data:'rbrke'),
    (Value:'10638';         Data:'rbrksld'),
    (Value:'10639';         Data:'lbrksld'),
    (Value:'1066';          Data:'HARDcy'),
    (Value:'10645';         Data:'gtlPar'),
    (Value:'10642';         Data:'rangd'),
    (Value:'10640';         Data:'rbrkslu'),
    (Value:'10641';         Data:'langd'),
    (Value:'10643';         Data:'lparlt'),
    (Value:'10644';         Data:'rpargt'),
    (Value:'10650';         Data:'vzigzag'),
    (Value:'10646';         Data:'ltrPar'),
    (Value:'1065';          Data:'SHCHcy'),
    (Value:'10652';         Data:'vangrt'),
    (Value:'10653';         Data:'angrtvbd'),
    (Value:'10665';         Data:'angmsdab'),
    (Value:'10662';         Data:'dwangle'),
    (Value:'10660';         Data:'ange'),
    (Value:'10661';         Data:'range'),
    (Value:'10663';         Data:'uwangle'),
    (Value:'10664';         Data:'angmsdaa'),
    (Value:'10668';         Data:'angmsdae'),
    (Value:'10666';         Data:'angmsdac'),
    (Value:'10667';         Data:'angmsdad'),
    (Value:'10669';         Data:'angmsdaf'),
    (Value:'1067';          Data:'Ycy'),
    (Value:'10820';         Data:'capand'),
    (Value:'10754';         Data:'xotime'),
    (Value:'10697';         Data:'boxbox'),
    (Value:'10681';         Data:'operp'),
    (Value:'10675';         Data:'raemptyv'),
    (Value:'10672';         Data:'bemptyv'),
    (Value:'10671';         Data:'angmsdah'),
    (Value:'10673';         Data:'demptyv'),
    (Value:'10674';         Data:'cemptyv'),
    (Value:'10678';         Data:'omid'),
    (Value:'10676';         Data:'laemptyv'),
    (Value:'10677';         Data:'ohbar'),
    (Value:'10679';         Data:'opar'),
    (Value:'1068';          Data:'SOFTcy'),
    (Value:'10689';         Data:'ogt'),
    (Value:'10686';         Data:'olcir'),
    (Value:'10683';         Data:'olcross'),
    (Value:'10684';         Data:'odsold'),
    (Value:'10687';         Data:'ofcir'),
    (Value:'10688';         Data:'olt'),
    (Value:'10691';         Data:'cirE'),
    (Value:'1069';          Data:'Ecy'),
    (Value:'10690';         Data:'cirscir'),
    (Value:'10692';         Data:'solb'),
    (Value:'10693';         Data:'bsolb'),
    (Value:'1072';          Data:'acy'),
    (Value:'10704,824';     Data:'NotRightTriangleBar'),
    (Value:'10702';         Data:'rtriltri'),
    (Value:'1070';          Data:'YUcy'),
    (Value:'10701';         Data:'trisb'),
    (Value:'10703,824';     Data:'NotLeftTriangleBar'),
    (Value:'10703';         Data:'LeftTriangleBar'),
    (Value:'10716';         Data:'iinfin'),
    (Value:'10704';         Data:'RightTriangleBar'),
    (Value:'1071';          Data:'YAcy'),
    (Value:'10717';         Data:'infintie'),
    (Value:'10718';         Data:'nvinfin'),
    (Value:'1074';          Data:'vcy'),
    (Value:'10725';         Data:'eqvparsl'),
    (Value:'10723';         Data:'eparsl'),
    (Value:'10724';         Data:'smeparsl'),
    (Value:'1073';          Data:'bcy'),
    (Value:'10731';         Data:'lozf'),
    (Value:'1075';          Data:'gcy'),
    (Value:'10740';         Data:'RuleDelayed'),
    (Value:'10742';         Data:'dsol'),
    (Value:'10752';         Data:'bigodot'),
    (Value:'10753';         Data:'xoplus'),
    (Value:'10793';         Data:'mcomma'),
    (Value:'10772';         Data:'npolint'),
    (Value:'10765';         Data:'fpartint'),
    (Value:'10758';         Data:'xsqcup'),
    (Value:'10756';         Data:'xuplus'),
    (Value:'1076';          Data:'dcy'),
    (Value:'10764';         Data:'qint'),
    (Value:'1077';          Data:'iecy'),
    (Value:'10768';         Data:'cirfnint'),
    (Value:'10769';         Data:'awint'),
    (Value:'10770';         Data:'rppolint'),
    (Value:'10771';         Data:'scpolint'),
    (Value:'10787';         Data:'plusacir'),
    (Value:'10775';         Data:'intlarhk'),
    (Value:'10773';         Data:'pointint'),
    (Value:'10774';         Data:'quatint'),
    (Value:'1078';          Data:'zhcy'),
    (Value:'10786';         Data:'pluscir'),
    (Value:'1079';          Data:'zcy'),
    (Value:'10788';         Data:'simplus'),
    (Value:'10789';         Data:'plusdu'),
    (Value:'10790';         Data:'plussim'),
    (Value:'10791';         Data:'plustwo'),
    (Value:'10807';         Data:'Otimes'),
    (Value:'10800';         Data:'timesd'),
    (Value:'10798';         Data:'roplus'),
    (Value:'10794';         Data:'minusdu'),
    (Value:'10797';         Data:'loplus'),
    (Value:'10799';         Data:'Cross'),
    (Value:'1080';          Data:'icy'),
    (Value:'10804';         Data:'lotimes'),
    (Value:'10801';         Data:'timesbar'),
    (Value:'10803';         Data:'smashp'),
    (Value:'10805';         Data:'rotimes'),
    (Value:'10806';         Data:'otimesas'),
    (Value:'10812';         Data:'iprod'),
    (Value:'1081';          Data:'jcy'),
    (Value:'10808';         Data:'odiv'),
    (Value:'10809';         Data:'triplus'),
    (Value:'10810';         Data:'triminus'),
    (Value:'10811';         Data:'tritime'),
    (Value:'10818';         Data:'ncup'),
    (Value:'10815';         Data:'amalg'),
    (Value:'10816';         Data:'capdot'),
    (Value:'10819';         Data:'ncap'),
    (Value:'1082';          Data:'kcy'),
    (Value:'10877';         Data:'les'),
    (Value:'10847';         Data:'wedbar'),
    (Value:'10832';         Data:'ccupssm'),
    (Value:'10825';         Data:'capbrcup'),
    (Value:'10822';         Data:'cupcap'),
    (Value:'10821';         Data:'cupor'),
    (Value:'10823';         Data:'capcup'),
    (Value:'10824';         Data:'cupbrcap'),
    (Value:'10828';         Data:'ccups'),
    (Value:'10826';         Data:'cupcup'),
    (Value:'10827';         Data:'capcap'),
    (Value:'10829';         Data:'ccaps'),
    (Value:'1083';          Data:'lcy'),
    (Value:'1084';          Data:'mcy'),
    (Value:'10837';         Data:'andand'),
    (Value:'10835';         Data:'And'),
    (Value:'10836';         Data:'Or'),
    (Value:'10838';         Data:'oror'),
    (Value:'10839';         Data:'orslope'),
    (Value:'10843';         Data:'orv'),
    (Value:'10840';         Data:'andslope'),
    (Value:'10842';         Data:'andv'),
    (Value:'10844';         Data:'andd'),
    (Value:'10845';         Data:'ord'),
    (Value:'10866';         Data:'pluse'),
    (Value:'10861';         Data:'congdot'),
    (Value:'10858';         Data:'simdot'),
    (Value:'1085';          Data:'ncy'),
    (Value:'10854';         Data:'sdote'),
    (Value:'1086';          Data:'ocy'),
    (Value:'10861,824';     Data:'ncongdot'),
    (Value:'10864,824';     Data:'napE'),
    (Value:'10862';         Data:'easter'),
    (Value:'10863';         Data:'apacir'),
    (Value:'10864';         Data:'apE'),
    (Value:'10865';         Data:'eplus'),
    (Value:'10872';         Data:'equivDD'),
    (Value:'10869';         Data:'Equal'),
    (Value:'10867';         Data:'Esim'),
    (Value:'10868';         Data:'Colone'),
    (Value:'1087';          Data:'pcy'),
    (Value:'10871';         Data:'eDDot'),
    (Value:'10875';         Data:'ltquest'),
    (Value:'10873';         Data:'ltcir'),
    (Value:'10874';         Data:'gtcir'),
    (Value:'10876';         Data:'gtquest'),
    (Value:'10877,824';     Data:'nles'),
    (Value:'10898';         Data:'glE'),
    (Value:'10887';         Data:'lne'),
    (Value:'10881';         Data:'lesdoto'),
    (Value:'10879';         Data:'lesdot'),
    (Value:'10878,824';     Data:'nges'),
    (Value:'10878';         Data:'ges'),
    (Value:'1088';          Data:'rcy'),
    (Value:'10880';         Data:'gesdot'),
    (Value:'10884';         Data:'gesdotol'),
    (Value:'10882';         Data:'gesdoto'),
    (Value:'10883';         Data:'lesdotor'),
    (Value:'10885';         Data:'lap'),
    (Value:'10886';         Data:'gap'),
    (Value:'10892';         Data:'gEl'),
    (Value:'1089';          Data:'scy'),
    (Value:'10888';         Data:'gne'),
    (Value:'10889';         Data:'lnap'),
    (Value:'10890';         Data:'gnap'),
    (Value:'10891';         Data:'lEg'),
    (Value:'10895';         Data:'lsimg'),
    (Value:'10893';         Data:'lsime'),
    (Value:'10894';         Data:'gsime'),
    (Value:'10896';         Data:'gsiml'),
    (Value:'10897';         Data:'lgE'),
    (Value:'10910';         Data:'simg'),
    (Value:'10903';         Data:'elsdot'),
    (Value:'10900';         Data:'gesles'),
    (Value:'10899';         Data:'lesges'),
    (Value:'1090';          Data:'tcy'),
    (Value:'10901';         Data:'els'),
    (Value:'10902';         Data:'egs'),
    (Value:'10906';         Data:'eg'),
    (Value:'10904';         Data:'egsdot'),
    (Value:'10905';         Data:'el'),
    (Value:'10909';         Data:'siml'),
    (Value:'1091';          Data:'ucy'),
    (Value:'10914';         Data:'GreaterGreater'),
    (Value:'10913,824';     Data:'NotNestedLessLess'),
    (Value:'10911';         Data:'simlE'),
    (Value:'10912';         Data:'simgE'),
    (Value:'10913';         Data:'LessLess'),
    (Value:'10914,824';     Data:'NotNestedGreaterGreater'),
    (Value:'10918';         Data:'ltcc'),
    (Value:'10916';         Data:'glj'),
    (Value:'10917';         Data:'gla'),
    (Value:'10919';         Data:'gtcc'),
    (Value:'1092';          Data:'fcy'),
    (Value:'120128';        Data:'Iopf'),
    (Value:'1119';          Data:'dzcy'),
    (Value:'10961';         Data:'csube'),
    (Value:'1094';          Data:'tscy'),
    (Value:'10928,824';     Data:'nsce'),
    (Value:'10924';         Data:'smte'),
    (Value:'10922';         Data:'smt'),
    (Value:'10921';         Data:'gescc'),
    (Value:'10923';         Data:'lat'),
    (Value:'10924,65024';   Data:'smtes'),
    (Value:'10926';         Data:'bumpE'),
    (Value:'10925,65024';   Data:'lates'),
    (Value:'10925';         Data:'late'),
    (Value:'10927,824';     Data:'npre'),
    (Value:'10927';         Data:'pre'),
    (Value:'10934';         Data:'scnE'),
    (Value:'10931';         Data:'prE'),
    (Value:'10928';         Data:'sce'),
    (Value:'1093';          Data:'khcy'),
    (Value:'10932';         Data:'scE'),
    (Value:'10933';         Data:'prnE'),
    (Value:'10937';         Data:'prnap'),
    (Value:'10935';         Data:'prap'),
    (Value:'10936';         Data:'scap'),
    (Value:'10938';         Data:'scnap'),
    (Value:'10939';         Data:'Pr'),
    (Value:'1095';          Data:'chcy'),
    (Value:'10945';         Data:'submult'),
    (Value:'10942';         Data:'supdot'),
    (Value:'10940';         Data:'Sc'),
    (Value:'10941';         Data:'subdot'),
    (Value:'10943';         Data:'subplus'),
    (Value:'10944';         Data:'supplus'),
    (Value:'10948';         Data:'supedot'),
    (Value:'10946';         Data:'supmult'),
    (Value:'10947';         Data:'subedot'),
    (Value:'10949,824';     Data:'nsubE'),
    (Value:'10949';         Data:'subE'),
    (Value:'10955';         Data:'subnE'),
    (Value:'10951';         Data:'subsim'),
    (Value:'10950,824';     Data:'nsupE'),
    (Value:'10950';         Data:'supE'),
    (Value:'10952';         Data:'supsim'),
    (Value:'10955,65024';   Data:'vsubnE'),
    (Value:'10959';         Data:'csub'),
    (Value:'10956,65024';   Data:'vsupnE'),
    (Value:'10956';         Data:'supnE'),
    (Value:'1096';          Data:'shcy'),
    (Value:'10960';         Data:'csup'),
    (Value:'10991';         Data:'cirmid'),
    (Value:'10971';         Data:'mlcp'),
    (Value:'10966';         Data:'supsup'),
    (Value:'10963';         Data:'subsup'),
    (Value:'10962';         Data:'csupe'),
    (Value:'10964';         Data:'supsub'),
    (Value:'10965';         Data:'subsub'),
    (Value:'10969';         Data:'forkv'),
    (Value:'10967';         Data:'suphsub'),
    (Value:'10968';         Data:'supdsub'),
    (Value:'1097';          Data:'shchcy'),
    (Value:'10970';         Data:'topfork'),
    (Value:'10985';         Data:'vBarv'),
    (Value:'10982';         Data:'Vdashl'),
    (Value:'1098';          Data:'hardcy'),
    (Value:'10980';         Data:'Dashv'),
    (Value:'10983';         Data:'Barv'),
    (Value:'10984';         Data:'vBar'),
    (Value:'10989';         Data:'bNot'),
    (Value:'10987';         Data:'Vbar'),
    (Value:'10988';         Data:'Not'),
    (Value:'1099';          Data:'ycy'),
    (Value:'10990';         Data:'rnmid'),
    (Value:'1106';          Data:'djcy'),
    (Value:'11005,8421';    Data:'nparsl'),
    (Value:'10994';         Data:'nhpar'),
    (Value:'10992';         Data:'midcir'),
    (Value:'10993';         Data:'topcir'),
    (Value:'10995';         Data:'parsim'),
    (Value:'1100';          Data:'softcy'),
    (Value:'1102';          Data:'yucy'),
    (Value:'11005';         Data:'parsl'),
    (Value:'1101';          Data:'ecy'),
    (Value:'1103';          Data:'yacy'),
    (Value:'1105';          Data:'iocy'),
    (Value:'1112';          Data:'jsercy'),
    (Value:'1109';          Data:'dscy'),
    (Value:'1107';          Data:'gjcy'),
    (Value:'1108';          Data:'jukcy'),
    (Value:'1110';          Data:'iukcy'),
    (Value:'1111';          Data:'yicy'),
    (Value:'1115';          Data:'tshcy'),
    (Value:'1113';          Data:'ljcy'),
    (Value:'1114';          Data:'njcy'),
    (Value:'1116';          Data:'kjcy'),
    (Value:'1118';          Data:'ubrcy'),
    (Value:'120074';        Data:'Gfr'),
    (Value:'119995';        Data:'fscr'),
    (Value:'119982';        Data:'Sscr'),
    (Value:'119973';        Data:'Jscr'),
    (Value:'119966';        Data:'Cscr'),
    (Value:'119964';        Data:'Ascr'),
    (Value:'119967';        Data:'Dscr'),
    (Value:'119970';        Data:'Gscr'),
    (Value:'119978';        Data:'Oscr'),
    (Value:'119974';        Data:'Kscr'),
    (Value:'119977';        Data:'Nscr'),
    (Value:'119979';        Data:'Pscr'),
    (Value:'119980';        Data:'Qscr'),
    (Value:'119988';        Data:'Yscr'),
    (Value:'119985';        Data:'Vscr'),
    (Value:'119983';        Data:'Tscr'),
    (Value:'119984';        Data:'Uscr'),
    (Value:'119986';        Data:'Wscr'),
    (Value:'119987';        Data:'Xscr'),
    (Value:'119991';        Data:'bscr'),
    (Value:'119989';        Data:'Zscr'),
    (Value:'119990';        Data:'ascr'),
    (Value:'119992';        Data:'cscr'),
    (Value:'119993';        Data:'dscr'),
    (Value:'120009';        Data:'tscr'),
    (Value:'120002';        Data:'mscr'),
    (Value:'119999';        Data:'jscr'),
    (Value:'119997';        Data:'hscr'),
    (Value:'119998';        Data:'iscr'),
    (Value:'120000';        Data:'kscr'),
    (Value:'120001';        Data:'lscr'),
    (Value:'120006';        Data:'qscr'),
    (Value:'120003';        Data:'nscr'),
    (Value:'120005';        Data:'pscr'),
    (Value:'120007';        Data:'rscr'),
    (Value:'120008';        Data:'sscr'),
    (Value:'120015';        Data:'zscr'),
    (Value:'120012';        Data:'wscr'),
    (Value:'120010';        Data:'uscr'),
    (Value:'120011';        Data:'vscr'),
    (Value:'120013';        Data:'xscr'),
    (Value:'120014';        Data:'yscr'),
    (Value:'120071';        Data:'Dfr'),
    (Value:'120068';        Data:'Afr'),
    (Value:'120069';        Data:'Bfr'),
    (Value:'120072';        Data:'Efr'),
    (Value:'120073';        Data:'Ffr'),
    (Value:'120102';        Data:'ifr'),
    (Value:'120089';        Data:'Vfr'),
    (Value:'120082';        Data:'Ofr'),
    (Value:'120079';        Data:'Lfr'),
    (Value:'120077';        Data:'Jfr'),
    (Value:'120078';        Data:'Kfr'),
    (Value:'120080';        Data:'Mfr'),
    (Value:'120081';        Data:'Nfr'),
    (Value:'120086';        Data:'Sfr'),
    (Value:'120083';        Data:'Pfr'),
    (Value:'120084';        Data:'Qfr'),
    (Value:'120087';        Data:'Tfr'),
    (Value:'120088';        Data:'Ufr'),
    (Value:'120096';        Data:'cfr'),
    (Value:'120092';        Data:'Yfr'),
    (Value:'120090';        Data:'Wfr'),
    (Value:'120091';        Data:'Xfr'),
    (Value:'120094';        Data:'afr'),
    (Value:'120095';        Data:'bfr'),
    (Value:'120099';        Data:'ffr'),
    (Value:'120097';        Data:'dfr'),
    (Value:'120098';        Data:'efr'),
    (Value:'120100';        Data:'gfr'),
    (Value:'120101';        Data:'hfr'),
    (Value:'120114';        Data:'ufr'),
    (Value:'120108';        Data:'ofr'),
    (Value:'120105';        Data:'lfr'),
    (Value:'120103';        Data:'jfr'),
    (Value:'120104';        Data:'kfr'),
    (Value:'120106';        Data:'mfr'),
    (Value:'120107';        Data:'nfr'),
    (Value:'120111';        Data:'rfr'),
    (Value:'120109';        Data:'pfr'),
    (Value:'120110';        Data:'qfr'),
    (Value:'120112';        Data:'sfr'),
    (Value:'120113';        Data:'tfr'),
    (Value:'120120';        Data:'Aopf'),
    (Value:'120117';        Data:'xfr'),
    (Value:'120115';        Data:'vfr'),
    (Value:'120116';        Data:'wfr'),
    (Value:'120118';        Data:'yfr'),
    (Value:'120119';        Data:'zfr'),
    (Value:'120124';        Data:'Eopf'),
    (Value:'120121';        Data:'Bopf'),
    (Value:'120123';        Data:'Dopf'),
    (Value:'120125';        Data:'Fopf'),
    (Value:'120126';        Data:'Gopf'),
    (Value:'206';           Data:'Icirc'),
    (Value:'164';           Data:'curren'),
    (Value:'120156';        Data:'kopf'),
    (Value:'120143';        Data:'Xopf'),
    (Value:'120134';        Data:'Oopf'),
    (Value:'120130';        Data:'Kopf'),
    (Value:'120129';        Data:'Jopf'),
    (Value:'120131';        Data:'Lopf'),
    (Value:'120132';        Data:'Mopf'),
    (Value:'120140';        Data:'Uopf'),
    (Value:'120138';        Data:'Sopf'),
    (Value:'120139';        Data:'Topf'),
    (Value:'120141';        Data:'Vopf'),
    (Value:'120142';        Data:'Wopf'),
    (Value:'120150';        Data:'eopf'),
    (Value:'120147';        Data:'bopf'),
    (Value:'120144';        Data:'Yopf'),
    (Value:'120146';        Data:'aopf'),
    (Value:'120148';        Data:'copf'),
    (Value:'120149';        Data:'dopf'),
    (Value:'120153';        Data:'hopf'),
    (Value:'120151';        Data:'fopf'),
    (Value:'120152';        Data:'gopf'),
    (Value:'120154';        Data:'iopf'),
    (Value:'120155';        Data:'jopf'),
    (Value:'120168';        Data:'wopf'),
    (Value:'120162';        Data:'qopf'),
    (Value:'120159';        Data:'nopf'),
    (Value:'120157';        Data:'lopf'),
    (Value:'120158';        Data:'mopf'),
    (Value:'120160';        Data:'oopf'),
    (Value:'120161';        Data:'popf'),
    (Value:'120165';        Data:'topf'),
    (Value:'120163';        Data:'ropf'),
    (Value:'120164';        Data:'sopf'),
    (Value:'120166';        Data:'uopf'),
    (Value:'120167';        Data:'vopf'),
    (Value:'123';           Data:'lbrace'),
    (Value:'120171';        Data:'zopf'),
    (Value:'120169';        Data:'xopf'),
    (Value:'120170';        Data:'yopf'),
    (Value:'12296';         Data:'lang'),
    (Value:'12297';         Data:'rang'),
    (Value:'161';           Data:'iexcl'),
    (Value:'124';           Data:'vert'),
    (Value:'125';           Data:'rbrace'),
    (Value:'162';           Data:'cent'),
    (Value:'163';           Data:'pound'),
    (Value:'183';           Data:'centerdot'),
    (Value:'174';           Data:'reg'),
    (Value:'168';           Data:'uml'),
    (Value:'166';           Data:'brvbar'),
    (Value:'165';           Data:'yen'),
    (Value:'166';           Data:'brvbar'),
    (Value:'167';           Data:'sect'),
    (Value:'171';           Data:'laquo'),
    (Value:'169';           Data:'copy'),
    (Value:'170';           Data:'ordf'),
    (Value:'172';           Data:'not'),
    (Value:'173';           Data:'shy'),
    (Value:'178';           Data:'sup2'),
    (Value:'175';           Data:'strns'),
    (Value:'174';           Data:'reg'),
    (Value:'175';           Data:'macr'),
    (Value:'176';           Data:'deg'),
    (Value:'177';           Data:'plusmn'),
    (Value:'180';           Data:'acute'),
    (Value:'179';           Data:'sup3'),
    (Value:'180';           Data:'acute'),
    (Value:'181';           Data:'micro'),
    (Value:'182';           Data:'para'),
    (Value:'194';           Data:'Acirc'),
    (Value:'188';           Data:'frac14'),
    (Value:'185';           Data:'sup1'),
    (Value:'184';           Data:'cedil'),
    (Value:'184';           Data:'cedil'),
    (Value:'186';           Data:'ordm'),
    (Value:'187';           Data:'raquo'),
    (Value:'191';           Data:'iquest'),
    (Value:'189';           Data:'frac12'),
    (Value:'190';           Data:'frac34'),
    (Value:'192';           Data:'Agrave'),
    (Value:'193';           Data:'Aacute'),
    (Value:'200';           Data:'Egrave'),
    (Value:'197';           Data:'Aring'),
    (Value:'195';           Data:'Atilde'),
    (Value:'196';           Data:'Auml'),
    (Value:'198';           Data:'AElig'),
    (Value:'199';           Data:'Ccedil'),
    (Value:'203';           Data:'Euml'),
    (Value:'201';           Data:'Eacute'),
    (Value:'202';           Data:'Ecirc'),
    (Value:'204';           Data:'Igrave'),
    (Value:'205';           Data:'Iacute'),
    (Value:'253';           Data:'yacute'),
    (Value:'229';           Data:'aring'),
    (Value:'217';           Data:'Ugrave'),
    (Value:'211';           Data:'Oacute'),
    (Value:'208';           Data:'ETH'),
    (Value:'207';           Data:'Iuml'),
    (Value:'209';           Data:'Ntilde'),
    (Value:'210';           Data:'Ograve'),
    (Value:'214';           Data:'Ouml'),
    (Value:'212';           Data:'Ocirc'),
    (Value:'213';           Data:'Otilde'),
    (Value:'215';           Data:'times'),
    (Value:'216';           Data:'Oslash'),
    (Value:'223';           Data:'szlig'),
    (Value:'220';           Data:'Uuml'),
    (Value:'218';           Data:'Uacute'),
    (Value:'219';           Data:'Ucirc'),
    (Value:'221';           Data:'Yacute'),
    (Value:'222';           Data:'THORN'),
    (Value:'226';           Data:'acirc'),
    (Value:'224';           Data:'agrave'),
    (Value:'225';           Data:'aacute'),
    (Value:'227';           Data:'atilde'),
    (Value:'228';           Data:'auml'),
    (Value:'241';           Data:'ntilde'),
    (Value:'235';           Data:'euml'),
    (Value:'232';           Data:'egrave'),
    (Value:'230';           Data:'aelig'),
    (Value:'231';           Data:'ccedil'),
    (Value:'233';           Data:'eacute'),
    (Value:'234';           Data:'ecirc'),
    (Value:'238';           Data:'icirc'),
    (Value:'236';           Data:'igrave'),
    (Value:'237';           Data:'iacute'),
    (Value:'239';           Data:'iuml'),
    (Value:'240';           Data:'eth'),
    (Value:'247';           Data:'divide'),
    (Value:'244';           Data:'ocirc'),
    (Value:'242';           Data:'ograve'),
    (Value:'243';           Data:'oacute'),
    (Value:'245';           Data:'otilde'),
    (Value:'246';           Data:'ouml'),
    (Value:'250';           Data:'uacute'),
    (Value:'248';           Data:'oslash'),
    (Value:'249';           Data:'ugrave'),
    (Value:'251';           Data:'ucirc'),
    (Value:'252';           Data:'uuml'),
    (Value:'279';           Data:'edot'),
    (Value:'265';           Data:'ccirc'),
    (Value:'259';           Data:'abreve'),
    (Value:'256';           Data:'Amacr'),
    (Value:'254';           Data:'thorn'),
    (Value:'255';           Data:'yuml'),
    (Value:'257';           Data:'amacr'),
    (Value:'258';           Data:'Abreve'),
    (Value:'262';           Data:'Cacute'),
    (Value:'260';           Data:'Aogon'),
    (Value:'261';           Data:'aogon'),
    (Value:'263';           Data:'cacute'),
    (Value:'264';           Data:'Ccirc'),
    (Value:'271';           Data:'dcaron'),
    (Value:'268';           Data:'Ccaron'),
    (Value:'266';           Data:'Cdot'),
    (Value:'267';           Data:'cdot'),
    (Value:'269';           Data:'ccaron'),
    (Value:'270';           Data:'Dcaron'),
    (Value:'274';           Data:'Emacr'),
    (Value:'272';           Data:'Dstrok'),
    (Value:'273';           Data:'dstrok'),
    (Value:'275';           Data:'emacr'),
    (Value:'278';           Data:'Edot'),
    (Value:'292';           Data:'Hcirc'),
    (Value:'285';           Data:'gcirc'),
    (Value:'282';           Data:'Ecaron'),
    (Value:'280';           Data:'Eogon'),
    (Value:'281';           Data:'eogon'),
    (Value:'283';           Data:'ecaron'),
    (Value:'284';           Data:'Gcirc'),
    (Value:'288';           Data:'Gdot'),
    (Value:'286';           Data:'Gbreve'),
    (Value:'287';           Data:'gbreve'),
    (Value:'289';           Data:'gdot'),
    (Value:'290';           Data:'Gcedil'),
    (Value:'298';           Data:'Imacr'),
    (Value:'295';           Data:'hstrok'),
    (Value:'293';           Data:'hcirc'),
    (Value:'294';           Data:'Hstrok'),
    (Value:'296';           Data:'Itilde'),
    (Value:'297';           Data:'itilde'),
    (Value:'303';           Data:'iogon'),
    (Value:'299';           Data:'imacr'),
    (Value:'302';           Data:'Iogon'),
    (Value:'304';           Data:'Idot'),
    (Value:'305';           Data:'inodot'),
    (Value:'8783,824';      Data:'nbumpe'),
    (Value:'8495';          Data:'escr'),
    (Value:'567';           Data:'jmath'),
    (Value:'351';           Data:'scedil'),
    (Value:'328';           Data:'ncaron'),
    (Value:'317';           Data:'Lcaron'),
    (Value:'311';           Data:'kcedil'),
    (Value:'308';           Data:'Jcirc'),
    (Value:'307';           Data:'ijlig'),
    (Value:'309';           Data:'jcirc'),
    (Value:'310';           Data:'Kcedil'),
    (Value:'314';           Data:'lacute'),
    (Value:'312';           Data:'kgreen'),
    (Value:'313';           Data:'Lacute'),
    (Value:'315';           Data:'Lcedil'),
    (Value:'316';           Data:'lcedil'),
    (Value:'322';           Data:'lstrok'),
    (Value:'32';            Data:'nbsp'),
    (Value:'318';           Data:'lcaron'),
    (Value:'319';           Data:'Lmidot'),
    (Value:'320';           Data:'lmidot'),
    (Value:'321';           Data:'Lstrok'),
    (Value:'325';           Data:'Ncedil'),
    (Value:'323';           Data:'Nacute'),
    (Value:'324';           Data:'nacute'),
    (Value:'326';           Data:'ncedil'),
    (Value:'327';           Data:'Ncaron'),
    (Value:'340';           Data:'Racute'),
    (Value:'333';           Data:'omacr'),
    (Value:'330';           Data:'ENG'),
    (Value:'329';           Data:'napos'),
    (Value:'33';            Data:'excl'),
    (Value:'331';           Data:'eng'),
    (Value:'332';           Data:'Omacr'),
    (Value:'338';           Data:'OElig'),
    (Value:'336';           Data:'Odblac'),
    (Value:'337';           Data:'odblac'),
    (Value:'339';           Data:'oelig'),
    (Value:'34';            Data:'quot'),
    (Value:'346';           Data:'Sacute'),
    (Value:'343';           Data:'rcedil'),
    (Value:'341';           Data:'racute'),
    (Value:'342';           Data:'Rcedil'),
    (Value:'344';           Data:'Rcaron'),
    (Value:'345';           Data:'rcaron'),
    (Value:'349';           Data:'scirc'),
    (Value:'347';           Data:'sacute'),
    (Value:'348';           Data:'Scirc'),
    (Value:'35';            Data:'num'),
    (Value:'350';           Data:'Scedil'),
    (Value:'372';           Data:'Wcirc'),
    (Value:'361';           Data:'utilde'),
    (Value:'356';           Data:'Tcaron'),
    (Value:'353';           Data:'scaron'),
    (Value:'352';           Data:'Scaron'),
    (Value:'354';           Data:'Tcedil'),
    (Value:'355';           Data:'tcedil'),
    (Value:'359';           Data:'tstrok'),
    (Value:'357';           Data:'tcaron'),
    (Value:'358';           Data:'Tstrok'),
    (Value:'36';            Data:'dollar'),
    (Value:'360';           Data:'Utilde'),
    (Value:'367';           Data:'uring'),
    (Value:'364';           Data:'Ubreve'),
    (Value:'362';           Data:'Umacr'),
    (Value:'363';           Data:'umacr'),
    (Value:'365';           Data:'ubreve'),
    (Value:'366';           Data:'Uring'),
    (Value:'37';            Data:'percnt'),
    (Value:'368';           Data:'Udblac'),
    (Value:'369';           Data:'udblac'),
    (Value:'370';           Data:'Uogon'),
    (Value:'371';           Data:'uogon'),
    (Value:'39';            Data:'apos'),
    (Value:'378';           Data:'zacute'),
    (Value:'375';           Data:'ycirc'),
    (Value:'373';           Data:'wcirc'),
    (Value:'374';           Data:'Ycirc'),
    (Value:'376';           Data:'Yuml'),
    (Value:'377';           Data:'Zacute'),
    (Value:'380';           Data:'zdot'),
    (Value:'379';           Data:'Zdot'),
    (Value:'38';            Data:'amp'),
    (Value:'381';           Data:'Zcaron'),
    (Value:'382';           Data:'zcaron'),
    (Value:'437';           Data:'imped'),
    (Value:'41';            Data:'rpar'),
    (Value:'40';            Data:'lpar'),
    (Value:'402';           Data:'fnof'),
    (Value:'42';            Data:'ast'),
    (Value:'43';            Data:'plus'),
    (Value:'46';            Data:'period'),
    (Value:'44';            Data:'comma'),
    (Value:'45';            Data:'minus'),
    (Value:'47';            Data:'sol'),
    (Value:'501';           Data:'gacute'),
    (Value:'8242';          Data:'prime'),
    (Value:'731';           Data:'ogon'),
    (Value:'63';            Data:'quest'),
    (Value:'60';            Data:'lt'),
    (Value:'59';            Data:'semi'),
    (Value:'58';            Data:'colon'),
    (Value:'60,8402';       Data:'nvlt'),
    (Value:'60';            Data:'LT'),
    (Value:'62,8402';       Data:'nvgt'),
    (Value:'61,8421';       Data:'bne'),
    (Value:'61';            Data:'equals'),
    (Value:'62';            Data:'GT'),
    (Value:'62';            Data:'gt'),
    (Value:'64260';         Data:'ffllig'),
    (Value:'64257';         Data:'filig'),
    (Value:'64';            Data:'commat'),
    (Value:'64256';         Data:'fflig'),
    (Value:'64258';         Data:'fllig'),
    (Value:'64259';         Data:'ffilig'),
    (Value:'728';           Data:'breve'),
    (Value:'710';           Data:'circ'),
    (Value:'711';           Data:'caron'),
    (Value:'729';           Data:'dot'),
    (Value:'730';           Data:'ring'),
    (Value:'8217';          Data:'rsquo'),
    (Value:'8208';          Data:'dash'),
    (Value:'785';           Data:'DownBreve'),
    (Value:'732';           Data:'tilde'),
    (Value:'733';           Data:'dblac'),
    (Value:'8204';          Data:'zwnj'),
    (Value:'8205';          Data:'zwj'),
    (Value:'8213';          Data:'horbar'),
    (Value:'8211';          Data:'ndash'),
    (Value:'8212';          Data:'mdash'),
    (Value:'8214';          Data:'Vert'),
    (Value:'8216';          Data:'lsquo'),
    (Value:'8225';          Data:'Dagger'),
    (Value:'8221';          Data:'rdquo'),
    (Value:'8218';          Data:'sbquo'),
    (Value:'8220';          Data:'ldquo'),
    (Value:'8222';          Data:'bdquo'),
    (Value:'8224';          Data:'dagger'),
    (Value:'8230';          Data:'mldr'),
    (Value:'8226';          Data:'bull'),
    (Value:'8229';          Data:'nldr'),
    (Value:'8240';          Data:'permil'),
    (Value:'8241';          Data:'pertenk'),
    (Value:'8461';          Data:'Hopf'),
    (Value:'8288';          Data:'NoBreak'),
    (Value:'8254';          Data:'oline'),
    (Value:'8245';          Data:'bprime'),
    (Value:'8243';          Data:'Prime'),
    (Value:'8244';          Data:'tprime'),
    (Value:'8249';          Data:'lsaquo'),
    (Value:'8250';          Data:'rsaquo'),
    (Value:'8260';          Data:'frasl'),
    (Value:'8257';          Data:'caret'),
    (Value:'8259';          Data:'hybull'),
    (Value:'8271';          Data:'bsemi'),
    (Value:'8279';          Data:'qprime'),
    (Value:'8412';          Data:'DotDot'),
    (Value:'8291';          Data:'ic'),
    (Value:'8289';          Data:'af'),
    (Value:'8290';          Data:'it'),
    (Value:'8364';          Data:'euro'),
    (Value:'8411';          Data:'tdot'),
    (Value:'8458';          Data:'gscr'),
    (Value:'8450';          Data:'Copf'),
    (Value:'8453';          Data:'incare'),
    (Value:'8459';          Data:'Hscr'),
    (Value:'8460';          Data:'Hfr'),
    (Value:'8474';          Data:'Qopf'),
    (Value:'8467';          Data:'ell'),
    (Value:'8464';          Data:'Iscr'),
    (Value:'8462';          Data:'planckh'),
    (Value:'8463';          Data:'hbar'),
    (Value:'8465';          Data:'Ifr'),
    (Value:'8466';          Data:'Lscr'),
    (Value:'8471';          Data:'copysr'),
    (Value:'8469';          Data:'Nopf'),
    (Value:'8470';          Data:'numero'),
    (Value:'8472';          Data:'wp'),
    (Value:'8473';          Data:'Popf'),
    (Value:'8484';          Data:'Zopf'),
    (Value:'8477';          Data:'Ropf'),
    (Value:'8475';          Data:'Rscr'),
    (Value:'8476';          Data:'Rfr'),
    (Value:'8478';          Data:'rx'),
    (Value:'8482';          Data:'trade'),
    (Value:'8489';          Data:'iiota'),
    (Value:'8487';          Data:'mho'),
    (Value:'8488';          Data:'Zfr'),
    (Value:'8492';          Data:'Bscr'),
    (Value:'8493';          Data:'Cfr'),
    (Value:'8661';          Data:'vArr'),
    (Value:'8613';          Data:'mapstoup'),
    (Value:'8541';          Data:'frac58'),
    (Value:'8519';          Data:'ee'),
    (Value:'8501';          Data:'aleph'),
    (Value:'8497';          Data:'Fscr'),
    (Value:'8496';          Data:'Escr'),
    (Value:'8499';          Data:'Mscr'),
    (Value:'8500';          Data:'oscr'),
    (Value:'8504';          Data:'daleth'),
    (Value:'8502';          Data:'beth'),
    (Value:'8503';          Data:'gimel'),
    (Value:'8517';          Data:'DD'),
    (Value:'8518';          Data:'dd'),
    (Value:'8535';          Data:'frac35'),
    (Value:'8532';          Data:'frac23'),
    (Value:'8520';          Data:'ii'),
    (Value:'8531';          Data:'frac13'),
    (Value:'8533';          Data:'frac15'),
    (Value:'8534';          Data:'frac25'),
    (Value:'8538';          Data:'frac56'),
    (Value:'8536';          Data:'frac45'),
    (Value:'8537';          Data:'frac16'),
    (Value:'8539';          Data:'frac18'),
    (Value:'8540';          Data:'frac38'),
    (Value:'8602';          Data:'nlarr'),
    (Value:'8596';          Data:'harr'),
    (Value:'8593';          Data:'uarr'),
    (Value:'8542';          Data:'frac78'),
    (Value:'8592';          Data:'larr'),
    (Value:'8594';          Data:'rarr'),
    (Value:'8595';          Data:'darr'),
    (Value:'8599';          Data:'nearr'),
    (Value:'8597';          Data:'varr'),
    (Value:'8598';          Data:'nwarr'),
    (Value:'8600';          Data:'searr'),
    (Value:'8601';          Data:'swarr'),
    (Value:'8608';          Data:'Rarr'),
    (Value:'8605';          Data:'rarrw'),
    (Value:'8603';          Data:'nrarr'),
    (Value:'8605,824';      Data:'nrarrw'),
    (Value:'8606';          Data:'Larr'),
    (Value:'8607';          Data:'Uarr'),
    (Value:'8611';          Data:'rarrtl'),
    (Value:'8609';          Data:'Darr'),
    (Value:'8610';          Data:'larrtl'),
    (Value:'8612';          Data:'LeftTeeArrow'),
    (Value:'8613';          Data:'UpTeeArrow'),
    (Value:'8640';          Data:'RightVector'),
    (Value:'8626';          Data:'ldsh'),
    (Value:'8619';          Data:'larrlp'),
    (Value:'8615';          Data:'DownTeeArrow'),
    (Value:'8614';          Data:'RightTeeArrow'),
    (Value:'8617';          Data:'larrhk'),
    (Value:'8618';          Data:'rarrhk'),
    (Value:'8622';          Data:'nharr'),
    (Value:'8620';          Data:'rarrlp'),
    (Value:'8621';          Data:'harrw'),
    (Value:'8624';          Data:'Lsh'),
    (Value:'8625';          Data:'rsh'),
    (Value:'8635';          Data:'orarr'),
    (Value:'8630';          Data:'cularr'),
    (Value:'8627';          Data:'rdsh'),
    (Value:'8629';          Data:'crarr'),
    (Value:'8631';          Data:'curarr'),
    (Value:'8634';          Data:'olarr'),
    (Value:'8638';          Data:'RightUpVector'),
    (Value:'8636';          Data:'lharu'),
    (Value:'8637';          Data:'lhard'),
    (Value:'8638';          Data:'uharr'),
    (Value:'8639';          Data:'uharl'),
    (Value:'8649';          Data:'rrarr'),
    (Value:'8643';          Data:'dharl'),
    (Value:'8642';          Data:'dharr'),
    (Value:'8640';          Data:'rharu'),
    (Value:'8641';          Data:'rhard'),
    (Value:'8642';          Data:'dharr'),
    (Value:'8643';          Data:'dharl'),
    (Value:'8646';          Data:'lrarr'),
    (Value:'8644';          Data:'rlarr'),
    (Value:'8645';          Data:'udarr'),
    (Value:'8647';          Data:'llarr'),
    (Value:'8648';          Data:'uuarr'),
    (Value:'8655';          Data:'nrArr'),
    (Value:'8652';          Data:'rlhar'),
    (Value:'8650';          Data:'ddarr'),
    (Value:'8651';          Data:'lrhar'),
    (Value:'8653';          Data:'nlArr'),
    (Value:'8654';          Data:'nhArr'),
    (Value:'8658';          Data:'rArr'),
    (Value:'8656';          Data:'lArr'),
    (Value:'8657';          Data:'uArr'),
    (Value:'8659';          Data:'dArr'),
    (Value:'8660';          Data:'hArr'),
    (Value:'8743';          Data:'and'),
    (Value:'8713';          Data:'notin'),
    (Value:'8701';          Data:'loarr'),
    (Value:'8666';          Data:'lAarr'),
    (Value:'8663';          Data:'neArr'),
    (Value:'8662';          Data:'nwArr'),
    (Value:'8664';          Data:'seArr'),
    (Value:'8665';          Data:'swArr'),
    (Value:'8676';          Data:'larrb'),
    (Value:'8667';          Data:'rAarr'),
    (Value:'8669';          Data:'zigrarr'),
    (Value:'8677';          Data:'rarrb'),
    (Value:'8693';          Data:'duarr'),
    (Value:'8706';          Data:'part'),
    (Value:'8704';          Data:'forall'),
    (Value:'8702';          Data:'roarr'),
    (Value:'8703';          Data:'hoarr'),
    (Value:'8705';          Data:'comp'),
    (Value:'8706,824';      Data:'npart'),
    (Value:'8709';          Data:'empty'),
    (Value:'8707';          Data:'exist'),
    (Value:'8708';          Data:'nexist'),
    (Value:'8711';          Data:'Del'),
    (Value:'8712';          Data:'in'),
    (Value:'8730';          Data:'radic'),
    (Value:'8722';          Data:'minus'),
    (Value:'8719';          Data:'prod'),
    (Value:'8715';          Data:'ni'),
    (Value:'8716';          Data:'notni'),
    (Value:'8720';          Data:'coprod'),
    (Value:'8721';          Data:'Sum'),
    (Value:'8726';          Data:'setmn'),
    (Value:'8723';          Data:'mp'),
    (Value:'8724';          Data:'plusdo'),
    (Value:'8727';          Data:'lowast'),
    (Value:'8728';          Data:'compfn'),
    (Value:'8737';          Data:'angmsd'),
    (Value:'8735';          Data:'angrt'),
    (Value:'8733';          Data:'prop'),
    (Value:'8734';          Data:'infin'),
    (Value:'8736,8402';     Data:'nang'),
    (Value:'8736';          Data:'ang'),
    (Value:'8740';          Data:'nmid'),
    (Value:'8738';          Data:'angsph'),
    (Value:'8739';          Data:'mid'),
    (Value:'8741';          Data:'par'),
    (Value:'8742';          Data:'npar'),
    (Value:'8765,817';      Data:'race'),
    (Value:'8753';          Data:'cwint'),
    (Value:'8747';          Data:'int'),
    (Value:'8745';          Data:'cap'),
    (Value:'8744';          Data:'or'),
    (Value:'8745,65024';    Data:'caps'),
    (Value:'8746,65024';    Data:'cups'),
    (Value:'8746';          Data:'cup'),
    (Value:'8750';          Data:'oint'),
    (Value:'8748';          Data:'Int'),
    (Value:'8749';          Data:'tint'),
    (Value:'8751';          Data:'Conint'),
    (Value:'8752';          Data:'Cconint'),
    (Value:'8759';          Data:'Colon'),
    (Value:'8756';          Data:'there4'),
    (Value:'8754';          Data:'cwconint'),
    (Value:'8755';          Data:'awconint'),
    (Value:'8757';          Data:'becaus'),
    (Value:'8758';          Data:'ratio'),
    (Value:'8763';          Data:'homtht'),
    (Value:'8760';          Data:'minusd'),
    (Value:'8762';          Data:'mDDot'),
    (Value:'8764,8402';     Data:'nvsim'),
    (Value:'8764';          Data:'sim'),
    (Value:'8774';          Data:'simne'),
    (Value:'8769';          Data:'nsim'),
    (Value:'8766';          Data:'ac'),
    (Value:'8765';          Data:'bsim'),
    (Value:'8766,819';      Data:'acE'),
    (Value:'8767';          Data:'acd'),
    (Value:'8768';          Data:'wr'),
    (Value:'8771';          Data:'sime'),
    (Value:'8770,824';      Data:'nesim'),
    (Value:'8770';          Data:'esim'),
    (Value:'8772';          Data:'nsime'),
    (Value:'8773';          Data:'cong'),
    (Value:'8779';          Data:'apid'),
    (Value:'8777';          Data:'nap'),
    (Value:'8775';          Data:'ncong'),
    (Value:'8776';          Data:'asymp'),
    (Value:'8778';          Data:'ape'),
    (Value:'8779,824';      Data:'napid'),
    (Value:'8781';          Data:'CupCap'),
    (Value:'8780';          Data:'bcong'),
    (Value:'8781,8402';     Data:'nvap'),
    (Value:'8782,824';      Data:'nbump'),
    (Value:'8782';          Data:'bump'),
    (Value:'8957';          Data:'notnivc'),
    (Value:'8862';          Data:'plusb'),
    (Value:'8822';          Data:'lg'),
    (Value:'8806,824';      Data:'nlE'),
    (Value:'8793';          Data:'wedgeq'),
    (Value:'8786';          Data:'efDot'),
    (Value:'8784,824';      Data:'nedot'),
    (Value:'8783';          Data:'bumpe'),
    (Value:'8784';          Data:'esdot'),
    (Value:'8785';          Data:'eDot'),
    (Value:'8789';          Data:'ecolon'),
    (Value:'8787';          Data:'erDot'),
    (Value:'8788';          Data:'colone'),
    (Value:'8790';          Data:'ecir'),
    (Value:'8791';          Data:'cire'),
    (Value:'8801';          Data:'equiv'),
    (Value:'8799';          Data:'equest'),
    (Value:'8794';          Data:'veeeq'),
    (Value:'8796';          Data:'trie'),
    (Value:'8800';          Data:'ne'),
    (Value:'8801,8421';     Data:'bnequiv'),
    (Value:'8804';          Data:'le'),
    (Value:'8802';          Data:'nequiv'),
    (Value:'8804,8402';     Data:'nvle'),
    (Value:'8805,8402';     Data:'nvge'),
    (Value:'8805';          Data:'ge'),
    (Value:'8811,8402';     Data:'nGt'),
    (Value:'8809,65024';    Data:'gvnE'),
    (Value:'8807';          Data:'gE'),
    (Value:'8806';          Data:'lE'),
    (Value:'8807,824';      Data:'ngE'),
    (Value:'8808,65024';    Data:'lvnE'),
    (Value:'8808';          Data:'lnE'),
    (Value:'8810,8402';     Data:'nLt'),
    (Value:'8809';          Data:'gnE'),
    (Value:'8810,824';      Data:'nLtv'),
    (Value:'8810';          Data:'ll'),
    (Value:'8811,824';      Data:'nGtv'),
    (Value:'8816';          Data:'nle'),
    (Value:'8813';          Data:'NotCupCap'),
    (Value:'8811';          Data:'gg'),
    (Value:'8812';          Data:'twixt'),
    (Value:'8814';          Data:'nlt'),
    (Value:'8815';          Data:'ngt'),
    (Value:'8819';          Data:'gsim'),
    (Value:'8817';          Data:'nge'),
    (Value:'8818';          Data:'lsim'),
    (Value:'8820';          Data:'nlsim'),
    (Value:'8821';          Data:'ngsim'),
    (Value:'8842,65024';    Data:'vsubne'),
    (Value:'8832';          Data:'npr'),
    (Value:'8827';          Data:'sc'),
    (Value:'8824';          Data:'ntlg'),
    (Value:'8823';          Data:'gl'),
    (Value:'8825';          Data:'ntgl'),
    (Value:'8826';          Data:'pr'),
    (Value:'8830';          Data:'prsim'),
    (Value:'8828';          Data:'prcue'),
    (Value:'8829';          Data:'sccue'),
    (Value:'8831,824';      Data:'NotSucceedsTilde'),
    (Value:'8831';          Data:'scsim'),
    (Value:'8836';          Data:'nsub'),
    (Value:'8834';          Data:'sub'),
    (Value:'8833';          Data:'nsc'),
    (Value:'8834,8402';     Data:'vnsub'),
    (Value:'8835,8402';     Data:'vnsup'),
    (Value:'8835';          Data:'sup'),
    (Value:'8839';          Data:'supe'),
    (Value:'8837';          Data:'nsup'),
    (Value:'8838';          Data:'sube'),
    (Value:'8840';          Data:'nsube'),
    (Value:'8841';          Data:'nsupe'),
    (Value:'8851,65024';    Data:'sqcaps'),
    (Value:'8847,824';      Data:'NotSquareSubset'),
    (Value:'8843';          Data:'supne'),
    (Value:'8842';          Data:'subne'),
    (Value:'8843,65024';    Data:'vsupne'),
    (Value:'8845';          Data:'cupdot'),
    (Value:'8846';          Data:'uplus'),
    (Value:'8848';          Data:'sqsup'),
    (Value:'8847';          Data:'sqsub'),
    (Value:'8848,824';      Data:'NotSquareSuperset'),
    (Value:'8849';          Data:'sqsube'),
    (Value:'8850';          Data:'sqsupe'),
    (Value:'8855';          Data:'otimes'),
    (Value:'8852';          Data:'sqcup'),
    (Value:'8851';          Data:'sqcap'),
    (Value:'8852,65024';    Data:'sqcups'),
    (Value:'8853';          Data:'oplus'),
    (Value:'8854';          Data:'ominus'),
    (Value:'8858';          Data:'ocir'),
    (Value:'8856';          Data:'osol'),
    (Value:'8857';          Data:'odot'),
    (Value:'8859';          Data:'oast'),
    (Value:'8861';          Data:'odash'),
    (Value:'8910';          Data:'cuvee'),
    (Value:'8885';          Data:'rtrie'),
    (Value:'8874';          Data:'Vvdash'),
    (Value:'8867';          Data:'dashv'),
    (Value:'8864';          Data:'timesb'),
    (Value:'8863';          Data:'minusb'),
    (Value:'8865';          Data:'sdotb'),
    (Value:'8866';          Data:'vdash'),
    (Value:'8871';          Data:'models'),
    (Value:'8868';          Data:'top'),
    (Value:'8869';          Data:'bot'),
    (Value:'8872';          Data:'vDash'),
    (Value:'8873';          Data:'Vdash'),
    (Value:'8880';          Data:'prurel'),
    (Value:'8877';          Data:'nvDash'),
    (Value:'8875';          Data:'VDash'),
    (Value:'8876';          Data:'nvdash'),
    (Value:'8878';          Data:'nVdash'),
    (Value:'8879';          Data:'nVDash'),
    (Value:'8884,8402';     Data:'nvltrie'),
    (Value:'8882';          Data:'vltri'),
    (Value:'8883';          Data:'vrtri'),
    (Value:'8884';          Data:'ltrie'),
    (Value:'8885,8402';     Data:'nvrtrie'),
    (Value:'8898';          Data:'xcap'),
    (Value:'8891';          Data:'veebar'),
    (Value:'8888';          Data:'mumap'),
    (Value:'8886';          Data:'origof'),
    (Value:'8887';          Data:'imof'),
    (Value:'8889';          Data:'hercon'),
    (Value:'8890';          Data:'intcal'),
    (Value:'8895';          Data:'lrtri'),
    (Value:'8893';          Data:'barvee'),
    (Value:'8894';          Data:'angrtvb'),
    (Value:'8896';          Data:'Wedge'),
    (Value:'8897';          Data:'Vee'),
    (Value:'8904';          Data:'bowtie'),
    (Value:'8901';          Data:'sdot'),
    (Value:'8899';          Data:'xcup'),
    (Value:'8900';          Data:'diam'),
    (Value:'8902';          Data:'Star'),
    (Value:'8903';          Data:'divonx'),
    (Value:'8907';          Data:'lthree'),
    (Value:'8905';          Data:'ltimes'),
    (Value:'8906';          Data:'rtimes'),
    (Value:'8908';          Data:'rthree'),
    (Value:'8909';          Data:'bsime'),
    (Value:'8934';          Data:'lnsim'),
    (Value:'8921,824';      Data:'nGg'),
    (Value:'8916';          Data:'fork'),
    (Value:'8913';          Data:'Sup'),
    (Value:'8911';          Data:'cuwed'),
    (Value:'8912';          Data:'Sub'),
    (Value:'8914';          Data:'Cap'),
    (Value:'8915';          Data:'Cup'),
    (Value:'8919';          Data:'gtdot'),
    (Value:'8917';          Data:'epar'),
    (Value:'8918';          Data:'ltdot'),
    (Value:'8920,824';      Data:'nLl'),
    (Value:'8920';          Data:'Ll'),
    (Value:'8926';          Data:'cuepr'),
    (Value:'8922';          Data:'leg'),
    (Value:'8921';          Data:'Gg'),
    (Value:'8922,65024';    Data:'lesg'),
    (Value:'8923,65024';    Data:'gesl'),
    (Value:'8923';          Data:'gel'),
    (Value:'8929';          Data:'nsccue'),
    (Value:'8927';          Data:'cuesc'),
    (Value:'8928';          Data:'nprcue'),
    (Value:'8930';          Data:'nsqsube'),
    (Value:'8931';          Data:'nsqsupe'),
    (Value:'8946';          Data:'disin'),
    (Value:'8940';          Data:'nltrie'),
    (Value:'8937';          Data:'scnsim'),
    (Value:'8935';          Data:'gnsim'),
    (Value:'8936';          Data:'prnsim'),
    (Value:'8938';          Data:'nltri'),
    (Value:'8939';          Data:'nrtri'),
    (Value:'8943';          Data:'ctdot'),
    (Value:'8941';          Data:'nrtrie'),
    (Value:'8942';          Data:'vellip'),
    (Value:'8944';          Data:'utdot'),
    (Value:'8945';          Data:'dtdot'),
    (Value:'8951';          Data:'notinvb'),
    (Value:'8949,824';      Data:'notindot'),
    (Value:'8947';          Data:'isinsv'),
    (Value:'8948';          Data:'isins'),
    (Value:'8949';          Data:'isindot'),
    (Value:'8950';          Data:'notinvc'),
    (Value:'8954';          Data:'nisd'),
    (Value:'8953,824';      Data:'notinE'),
    (Value:'8953';          Data:'isinE'),
    (Value:'8955';          Data:'xnis'),
    (Value:'8956';          Data:'nis'),
    (Value:'955';           Data:'lambda'),
    (Value:'9191';          Data:'elinters'),
    (Value:'9005';          Data:'cylcty'),
    (Value:'8975';          Data:'ulcrop'),
    (Value:'8969';          Data:'rceil'),
    (Value:'8965';          Data:'barwed'),
    (Value:'8958';          Data:'notnivb'),
    (Value:'8966';          Data:'Barwed'),
    (Value:'8968';          Data:'lceil'),
    (Value:'8972';          Data:'drcrop'),
    (Value:'8970';          Data:'lfloor'),
    (Value:'8971';          Data:'rfloor'),
    (Value:'8973';          Data:'dlcrop'),
    (Value:'8974';          Data:'urcrop'),
    (Value:'8988';          Data:'ulcorn'),
    (Value:'8979';          Data:'profsurf'),
    (Value:'8976';          Data:'bnot'),
    (Value:'8978';          Data:'profline'),
    (Value:'8981';          Data:'telrec'),
    (Value:'8982';          Data:'target'),
    (Value:'8991';          Data:'drcorn'),
    (Value:'8989';          Data:'urcorn'),
    (Value:'8990';          Data:'dlcorn'),
    (Value:'8994';          Data:'frown'),
    (Value:'8995';          Data:'smile'),
    (Value:'9141';          Data:'bbrk'),
    (Value:'91';            Data:'lsqb'),
    (Value:'9021';          Data:'ovbar'),
    (Value:'9006';          Data:'profalar'),
    (Value:'9014';          Data:'topbot'),
    (Value:'9023';          Data:'solbar'),
    (Value:'9084';          Data:'angzarr'),
    (Value:'9137';          Data:'rmoust'),
    (Value:'913';           Data:'Alpha'),
    (Value:'9136';          Data:'lmoust'),
    (Value:'914';           Data:'Beta'),
    (Value:'9140';          Data:'tbrk'),
    (Value:'9180';          Data:'OverParenthesis'),
    (Value:'916';           Data:'Delta'),
    (Value:'9142';          Data:'bbrktbrk'),
    (Value:'915';           Data:'Gamma'),
    (Value:'917';           Data:'Epsilon'),
    (Value:'918';           Data:'Zeta'),
    (Value:'9183';          Data:'UnderBrace'),
    (Value:'9181';          Data:'UnderParenthesis'),
    (Value:'9182';          Data:'OverBrace'),
    (Value:'9186';          Data:'trpezium'),
    (Value:'919';           Data:'Eta'),
    (Value:'9416';          Data:'oS'),
    (Value:'927';           Data:'Omicron'),
    (Value:'9226';          Data:'NewLine'),
    (Value:'921';           Data:'Iota'),
    (Value:'92';            Data:'bsol'),
    (Value:'920';           Data:'Theta'),
    (Value:'922';           Data:'Kappa'),
    (Value:'9225';          Data:'Tab'),
    (Value:'925';           Data:'Nu'),
    (Value:'923';           Data:'Lambda'),
    (Value:'924';           Data:'Mu'),
    (Value:'9251';          Data:'blank'),
    (Value:'926';           Data:'Xi'),
    (Value:'933';           Data:'Upsilon'),
    (Value:'93';            Data:'rsqb'),
    (Value:'928';           Data:'Pi'),
    (Value:'929';           Data:'Rho'),
    (Value:'931';           Data:'Sigma'),
    (Value:'932';           Data:'Tau'),
    (Value:'936';           Data:'Psi'),
    (Value:'934';           Data:'Phi'),
    (Value:'935';           Data:'Chi'),
    (Value:'937';           Data:'Omega'),
    (Value:'94';            Data:'Hat'),
    (Value:'9496';          Data:'boxul'),
    (Value:'948';           Data:'delta'),
    (Value:'947';           Data:'gamma'),
    (Value:'945';           Data:'alpha'),
    (Value:'946';           Data:'beta'),
    (Value:'9472';          Data:'boxh'),
    (Value:'9474';          Data:'boxv'),
    (Value:'949';           Data:'epsilon'),
    (Value:'9484';          Data:'boxdr'),
    (Value:'9488';          Data:'boxdl'),
    (Value:'949';           Data:'epsilon'),
    (Value:'9492';          Data:'boxur'),
    (Value:'9516';          Data:'boxhd'),
    (Value:'9500';          Data:'boxvr'),
    (Value:'95';            Data:'lowbar'),
    (Value:'950';           Data:'zeta'),
    (Value:'9508';          Data:'boxvl'),
    (Value:'951';           Data:'eta'),
    (Value:'953';           Data:'iota'),
    (Value:'952';           Data:'theta'),
    (Value:'9524';          Data:'boxhu'),
    (Value:'9532';          Data:'boxvh'),
    (Value:'954';           Data:'kappa'),
    (Value:'9642';          Data:'squf'),
    (Value:'9572';          Data:'boxHd'),
    (Value:'9561';          Data:'boxUr'),
    (Value:'9556';          Data:'boxDR'),
    (Value:'9553';          Data:'boxV'),
    (Value:'9552';          Data:'boxH'),
    (Value:'9554';          Data:'boxdR'),
    (Value:'9555';          Data:'boxDr'),
    (Value:'9559';          Data:'boxDL'),
    (Value:'9557';          Data:'boxdL'),
    (Value:'9558';          Data:'boxDl'),
    (Value:'956';           Data:'mu'),
    (Value:'9560';          Data:'boxuR'),
    (Value:'9567';          Data:'boxVr'),
    (Value:'9564';          Data:'boxUl'),
    (Value:'9562';          Data:'boxUR'),
    (Value:'9563';          Data:'boxuL'),
    (Value:'9565';          Data:'boxUL'),
    (Value:'9566';          Data:'boxvR'),
    (Value:'957';           Data:'nu'),
    (Value:'9568';          Data:'boxVR'),
    (Value:'9569';          Data:'boxvL'),
    (Value:'9570';          Data:'boxVl'),
    (Value:'9571';          Data:'boxVL'),
    (Value:'960';           Data:'pi'),
    (Value:'9578';          Data:'boxvH'),
    (Value:'9575';          Data:'boxHu'),
    (Value:'9573';          Data:'boxhD'),
    (Value:'9574';          Data:'boxHD'),
    (Value:'9576';          Data:'boxhU'),
    (Value:'9577';          Data:'boxHU'),
    (Value:'9580';          Data:'boxVH'),
    (Value:'9579';          Data:'boxVh'),
    (Value:'958';           Data:'xi'),
    (Value:'959';           Data:'omicron'),
    (Value:'96';            Data:'grave'),
    (Value:'9618';          Data:'blk12'),
    (Value:'9608';          Data:'block'),
    (Value:'9600';          Data:'uhblk'),
    (Value:'9604';          Data:'lhblk'),
    (Value:'961';           Data:'rho'),
    (Value:'9617';          Data:'blk14'),
    (Value:'963';           Data:'sigma'),
    (Value:'9619';          Data:'blk34'),
    (Value:'962';           Data:'sigmaf'),
    (Value:'9633';          Data:'squ'),
    (Value:'964';           Data:'tau'),
    (Value:'9720';          Data:'ultri'),
    (Value:'9661';          Data:'xdtri'),
    (Value:'9651';          Data:'xutri'),
    (Value:'9646';          Data:'marker'),
    (Value:'9643';          Data:'EmptyVerySmallSquare'),
    (Value:'9645';          Data:'rect'),
    (Value:'9649';          Data:'fltns'),
    (Value:'965';           Data:'upsilon'),
    (Value:'9656';          Data:'rtrif'),
    (Value:'9652';          Data:'utrif'),
    (Value:'9653';          Data:'utri'),
    (Value:'9657';          Data:'rtri'),
    (Value:'966';           Data:'phi'),
    (Value:'9674';          Data:'loz'),
    (Value:'9666';          Data:'ltrif'),
    (Value:'9662';          Data:'dtrif'),
    (Value:'9663';          Data:'dtri'),
    (Value:'9667';          Data:'ltri'),
    (Value:'967';           Data:'chi'),
    (Value:'969';           Data:'omega'),
    (Value:'9675';          Data:'cir'),
    (Value:'968';           Data:'psi'),
    (Value:'9708';          Data:'tridot'),
    (Value:'9711';          Data:'xcirc'),
    (Value:'981';           Data:'straightphi'),
    (Value:'9734';          Data:'star'),
    (Value:'9723';          Data:'EmptySmallSquare'),
    (Value:'9721';          Data:'urtri'),
    (Value:'9722';          Data:'lltri'),
    (Value:'9724';          Data:'FilledSmallSquare'),
    (Value:'9733';          Data:'starf'),
    (Value:'978';           Data:'upsih'),
    (Value:'9742';          Data:'phone'),
    (Value:'977';           Data:'thetasym'),
    (Value:'9792';          Data:'female'),
    (Value:'9794';          Data:'male'),
    (Value:'9834';          Data:'sung'),
    (Value:'9827';          Data:'clubs'),
    (Value:'982';           Data:'piv'),
    (Value:'9824';          Data:'spades'),
    (Value:'9829';          Data:'hearts'),
    (Value:'9830';          Data:'diams'),
    (Value:'9839';          Data:'sharp'),
    (Value:'9837';          Data:'flat'),
    (Value:'9838';          Data:'natur'),
    (Value:'988';           Data:'Gammad'),
    (Value:'989';           Data:'gammad')
  );

{$ELSE}

{$IFNDEF TextRemapEntries}
{$DEFINE TextRemapEntries}
type
  TTextRemapEntry = record
    Value : PChar;
    Data : PChar;
  end;
{$ENDIF}

procedure UTF2HTMLdata; assembler; 
asm
	db '306',0,        'IJlig',0
	db '10920',0,      'lescc',0
	db '10670',0,      'angmsdag',0
	db '10553',0,      'larrpl',0
	db '1044',0,       'Dcy',0
	db '10231',0,      'xharr',0
	db '1014',0,       'bepsi',0
	db '10072',0,      'VerticalSeparator',0
	db '10007',0,      'cross',0
	db '10003',0,      'check',0
	db '10016',0,      'malt',0
	db '10038',0,      'sext',0
	db '10098',0,      'lbbrk',0
	db '1008',0,       'kappav',0
	db '1009',0,       'rhov',0
	db '10099',0,      'rbbrk',0
	db '1013',0,       'epsiv',0
	db '10218',0,      'Lang',0
	db '102,106',0,    'fjlig',0
	db '10184',0,      'bsolhsub',0
	db '10185',0,      'suphsol',0
	db '10214',0,      'lobrk',0
	db '10215',0,      'robrk',0
	db '10221',0,      'roang',0
	db '10219',0,      'Rang',0
	db '10220',0,      'loang',0
	db '10229',0,      'xlarr',0
	db '10230',0,      'xrarr',0
	db '1031',0,       'YIcy',0
	db '1025',0,       'IOcy',0
	db '10234',0,      'xhArr',0
	db '10232',0,      'xlArr',0
	db '10233',0,      'xrArr',0
	db '10236',0,      'xmap',0
	db '10239',0,      'dzigrarr',0
	db '1028',0,       'Jukcy',0
	db '1026',0,       'DJcy',0
	db '1027',0,       'GJcy',0
	db '1029',0,       'DScy',0
	db '1030',0,       'Iukcy',0
	db '1038',0,       'Ubrcy',0
	db '1034',0,       'NJcy',0
	db '1032',0,       'Jsercy',0
	db '1033',0,       'LJcy',0
	db '1035',0,       'TSHcy',0
	db '1036',0,       'KJcy',0
	db '1041',0,       'Bcy',0
	db '1039',0,       'DZcy',0
	db '1040',0,       'Acy',0
	db '1042',0,       'Vcy',0
	db '1043',0,       'Gcy',0
	db '10522',0,      'ratail',0
	db '10508',0,      'lbarr',0
	db '1049',0,       'Jcy',0
	db '1046',0,       'ZHcy',0
	db '1045',0,       'IEcy',0
	db '1047',0,       'Zcy',0
	db '1048',0,       'Icy',0
	db '1050',0,       'Kcy',0
	db '10498',0,      'nvlArr',0
	db '10499',0,      'nvrArr',0
	db '10500',0,      'nvHarr',0
	db '10501',0,      'Map',0
	db '10513',0,      'DDotrahd',0
	db '10510',0,      'lBarr',0
	db '10509',0,      'rbarr',0
	db '1051',0,       'Lcy',0
	db '10511',0,      'rBarr',0
	db '10512',0,      'RBarr',0
	db '10518',0,      'Rarrtl',0
	db '10514',0,      'UpArrowBar',0
	db '10515',0,      'DownArrowBar',0
	db '1052',0,       'Mcy',0
	db '10521',0,      'latail',0
	db '10535',0,      'nwnear',0
	db '10528',0,      'rarrbfs',0
	db '10525',0,      'larrfs',0
	db '10523',0,      'lAtail',0
	db '10524',0,      'rAtail',0
	db '10526',0,      'rarrfs',0
	db '10527',0,      'larrbfs',0
	db '10532',0,      'nearhk',0
	db '1053',0,       'Ncy',0
	db '10531',0,      'nwarhk',0
	db '10533',0,      'searhk',0
	db '10534',0,      'swarhk',0
	db '10547',0,      'rarrc',0
	db '10538',0,      'swnwar',0
	db '10536',0,      'toea',0
	db '10537',0,      'tosa',0
	db '1054',0,       'Ocy',0
	db '10547,824',0,  'nrarrc',0
	db '10550',0,      'ldca',0
	db '10549',0,      'cudarrr',0
	db '1055',0,       'Pcy',0
	db '10551',0,      'rdca',0
	db '10552',0,      'cudarrl',0
	db '10608',0,      'RoundImplies',0
	db '10586',0,      'LeftTeeVector',0
	db '10575',0,      'RightUpDownVector',0
	db '10568',0,      'harrcir',0
	db '10557',0,      'cularrp',0
	db '10556',0,      'curarrm',0
	db '1056',0,       'Rcy',0
	db '10565',0,      'rarrpl',0
	db '10570',0,      'lurdshar',0
	db '10569',0,      'Uarrocir',0
	db '1057',0,       'Scy',0
	db '10571',0,      'ldrushar',0
	db '10574',0,      'LeftRightVector',0
	db '10580',0,      'RightUpVectorBar',0
	db '10578',0,      'LeftVectorBar',0
	db '10576',0,      'DownLeftRightVector',0
	db '10577',0,      'LeftUpDownVector',0
	db '10579',0,      'RightVectorBar',0
	db '1058',0,       'Tcy',0
	db '10583',0,      'DownRightVectorBar',0
	db '10581',0,      'RightDownVectorBar',0
	db '10582',0,      'DownLeftVectorBar',0
	db '10584',0,      'LeftUpVectorBar',0
	db '10585',0,      'LeftDownVectorBar',0
	db '10597',0,      'dHar',0
	db '10591',0,      'DownRightTeeVector',0
	db '10589',0,      'RightDownTeeVector',0
	db '10587',0,      'RightTeeVector',0
	db '10588',0,      'RightUpTeeVector',0
	db '1059',0,       'Ucy',0
	db '10590',0,      'DownLeftTeeVector',0
	db '10594',0,      'lHar',0
	db '10592',0,      'LeftUpTeeVector',0
	db '10593',0,      'LeftDownTeeVector',0
	db '10595',0,      'uHar',0
	db '10596',0,      'rHar',0
	db '10602',0,      'lharul',0
	db '1060',0,       'Fcy',0
	db '10598',0,      'luruhar',0
	db '10599',0,      'ldrdhar',0
	db '10600',0,      'ruluhar',0
	db '10601',0,      'rdldhar',0
	db '10605',0,      'lrhard',0
	db '10603',0,      'llhard',0
	db '10604',0,      'rharul',0
	db '10606',0,      'udhar',0
	db '10607',0,      'duhar',0
	db '1064',0,       'SHcy',0
	db '10620',0,      'lfisht',0
	db '10613',0,      'rarrap',0
	db '10610',0,      'simrarr',0
	db '10609',0,      'erarr',0
	db '1061',0,       'KHcy',0
	db '10611',0,      'larrsim',0
	db '10612',0,      'rarrsim',0
	db '10617',0,      'subrarr',0
	db '10614',0,      'ltlarr',0
	db '10616',0,      'gtrarr',0
	db '10619',0,      'suplarr',0
	db '1062',0,       'TScy',0
	db '10630',0,      'ropar',0
	db '10623',0,      'dfisht',0
	db '10621',0,      'rfisht',0
	db '10622',0,      'ufisht',0
	db '10629',0,      'lopar',0
	db '1063',0,       'CHcy',0
	db '10637',0,      'lbrkslu',0
	db '10635',0,      'lbrke',0
	db '10636',0,      'rbrke',0
	db '10638',0,      'rbrksld',0
	db '10639',0,      'lbrksld',0
	db '1066',0,       'HARDcy',0
	db '10645',0,      'gtlPar',0
	db '10642',0,      'rangd',0
	db '10640',0,      'rbrkslu',0
	db '10641',0,      'langd',0
	db '10643',0,      'lparlt',0
	db '10644',0,      'rpargt',0
	db '10650',0,      'vzigzag',0
	db '10646',0,      'ltrPar',0
	db '1065',0,       'SHCHcy',0
	db '10652',0,      'vangrt',0
	db '10653',0,      'angrtvbd',0
	db '10665',0,      'angmsdab',0
	db '10662',0,      'dwangle',0
	db '10660',0,      'ange',0
	db '10661',0,      'range',0
	db '10663',0,      'uwangle',0
	db '10664',0,      'angmsdaa',0
	db '10668',0,      'angmsdae',0
	db '10666',0,      'angmsdac',0
	db '10667',0,      'angmsdad',0
	db '10669',0,      'angmsdaf',0
	db '1067',0,       'Ycy',0
	db '10820',0,      'capand',0
	db '10754',0,      'xotime',0
	db '10697',0,      'boxbox',0
	db '10681',0,      'operp',0
	db '10675',0,      'raemptyv',0
	db '10672',0,      'bemptyv',0
	db '10671',0,      'angmsdah',0
	db '10673',0,      'demptyv',0
	db '10674',0,      'cemptyv',0
	db '10678',0,      'omid',0
	db '10676',0,      'laemptyv',0
	db '10677',0,      'ohbar',0
	db '10679',0,      'opar',0
	db '1068',0,       'SOFTcy',0
	db '10689',0,      'ogt',0
	db '10686',0,      'olcir',0
	db '10683',0,      'olcross',0
	db '10684',0,      'odsold',0
	db '10687',0,      'ofcir',0
	db '10688',0,      'olt',0
	db '10691',0,      'cirE',0
	db '1069',0,       'Ecy',0
	db '10690',0,      'cirscir',0
	db '10692',0,      'solb',0
	db '10693',0,      'bsolb',0
	db '1072',0,       'acy',0
	db '10704,824',0,  'NotRightTriangleBar',0
	db '10702',0,      'rtriltri',0
	db '1070',0,       'YUcy',0
	db '10701',0,      'trisb',0
	db '10703,824',0,  'NotLeftTriangleBar',0
	db '10703',0,      'LeftTriangleBar',0
	db '10716',0,      'iinfin',0
	db '10704',0,      'RightTriangleBar',0
	db '1071',0,       'YAcy',0
	db '10717',0,      'infintie',0
	db '10718',0,      'nvinfin',0
	db '1074',0,       'vcy',0
	db '10725',0,      'eqvparsl',0
	db '10723',0,      'eparsl',0
	db '10724',0,      'smeparsl',0
	db '1073',0,       'bcy',0
	db '10731',0,      'lozf',0
	db '1075',0,       'gcy',0
	db '10740',0,      'RuleDelayed',0
	db '10742',0,      'dsol',0
	db '10752',0,      'bigodot',0
	db '10753',0,      'xoplus',0
	db '10793',0,      'mcomma',0
	db '10772',0,      'npolint',0
	db '10765',0,      'fpartint',0
	db '10758',0,      'xsqcup',0
	db '10756',0,      'xuplus',0
	db '1076',0,       'dcy',0
	db '10764',0,      'qint',0
	db '1077',0,       'iecy',0
	db '10768',0,      'cirfnint',0
	db '10769',0,      'awint',0
	db '10770',0,      'rppolint',0
	db '10771',0,      'scpolint',0
	db '10787',0,      'plusacir',0
	db '10775',0,      'intlarhk',0
	db '10773',0,      'pointint',0
	db '10774',0,      'quatint',0
	db '1078',0,       'zhcy',0
	db '10786',0,      'pluscir',0
	db '1079',0,       'zcy',0
	db '10788',0,      'simplus',0
	db '10789',0,      'plusdu',0
	db '10790',0,      'plussim',0
	db '10791',0,      'plustwo',0
	db '10807',0,      'Otimes',0
	db '10800',0,      'timesd',0
	db '10798',0,      'roplus',0
	db '10794',0,      'minusdu',0
	db '10797',0,      'loplus',0
	db '10799',0,      'Cross',0
	db '1080',0,       'icy',0
	db '10804',0,      'lotimes',0
	db '10801',0,      'timesbar',0
	db '10803',0,      'smashp',0
	db '10805',0,      'rotimes',0
	db '10806',0,      'otimesas',0
	db '10812',0,      'iprod',0
	db '1081',0,       'jcy',0
	db '10808',0,      'odiv',0
	db '10809',0,      'triplus',0
	db '10810',0,      'triminus',0
	db '10811',0,      'tritime',0
	db '10818',0,      'ncup',0
	db '10815',0,      'amalg',0
	db '10816',0,      'capdot',0
	db '10819',0,      'ncap',0
	db '1082',0,       'kcy',0
	db '10877',0,      'les',0
	db '10847',0,      'wedbar',0
	db '10832',0,      'ccupssm',0
	db '10825',0,      'capbrcup',0
	db '10822',0,      'cupcap',0
	db '10821',0,      'cupor',0
	db '10823',0,      'capcup',0
	db '10824',0,      'cupbrcap',0
	db '10828',0,      'ccups',0
	db '10826',0,      'cupcup',0
	db '10827',0,      'capcap',0
	db '10829',0,      'ccaps',0
	db '1083',0,       'lcy',0
	db '1084',0,       'mcy',0
	db '10837',0,      'andand',0
	db '10835',0,      'And',0
	db '10836',0,      'Or',0
	db '10838',0,      'oror',0
	db '10839',0,      'orslope',0
	db '10843',0,      'orv',0
	db '10840',0,      'andslope',0
	db '10842',0,      'andv',0
	db '10844',0,      'andd',0
	db '10845',0,      'ord',0
	db '10866',0,      'pluse',0
	db '10861',0,      'congdot',0
	db '10858',0,      'simdot',0
	db '1085',0,       'ncy',0
	db '10854',0,      'sdote',0
	db '1086',0,       'ocy',0
	db '10861,824',0,  'ncongdot',0
	db '10864,824',0,  'napE',0
	db '10862',0,      'easter',0
	db '10863',0,      'apacir',0
	db '10864',0,      'apE',0
	db '10865',0,      'eplus',0
	db '10872',0,      'equivDD',0
	db '10869',0,      'Equal',0
	db '10867',0,      'Esim',0
	db '10868',0,      'Colone',0
	db '1087',0,       'pcy',0
	db '10871',0,      'eDDot',0
	db '10875',0,      'ltquest',0
	db '10873',0,      'ltcir',0
	db '10874',0,      'gtcir',0
	db '10876',0,      'gtquest',0
	db '10877,824',0,  'nles',0
	db '10898',0,      'glE',0
	db '10887',0,      'lne',0
	db '10881',0,      'lesdoto',0
	db '10879',0,      'lesdot',0
	db '10878,824',0,  'nges',0
	db '10878',0,      'ges',0
	db '1088',0,       'rcy',0
	db '10880',0,      'gesdot',0
	db '10884',0,      'gesdotol',0
	db '10882',0,      'gesdoto',0
	db '10883',0,      'lesdotor',0
	db '10885',0,      'lap',0
	db '10886',0,      'gap',0
	db '10892',0,      'gEl',0
	db '1089',0,       'scy',0
	db '10888',0,      'gne',0
	db '10889',0,      'lnap',0
	db '10890',0,      'gnap',0
	db '10891',0,      'lEg',0
	db '10895',0,      'lsimg',0
	db '10893',0,      'lsime',0
	db '10894',0,      'gsime',0
	db '10896',0,      'gsiml',0
	db '10897',0,      'lgE',0
	db '10910',0,      'simg',0
	db '10903',0,      'elsdot',0
	db '10900',0,      'gesles',0
	db '10899',0,      'lesges',0
	db '1090',0,       'tcy',0
	db '10901',0,      'els',0
	db '10902',0,      'egs',0
	db '10906',0,      'eg',0
	db '10904',0,      'egsdot',0
	db '10905',0,      'el',0
	db '10909',0,      'siml',0
	db '1091',0,       'ucy',0
	db '10914',0,      'GreaterGreater',0
	db '10913,824',0,  'NotNestedLessLess',0
	db '10911',0,      'simlE',0
	db '10912',0,      'simgE',0
	db '10913',0,      'LessLess',0
	db '10914,824',0,  'NotNestedGreaterGreater',0
	db '10918',0,      'ltcc',0
	db '10916',0,      'glj',0
	db '10917',0,      'gla',0
	db '10919',0,      'gtcc',0
	db '1092',0,       'fcy',0
	db '120128',0,     'Iopf',0
	db '1119',0,       'dzcy',0
	db '10961',0,      'csube',0
	db '1094',0,       'tscy',0
	db '10928,824',0,  'nsce',0
	db '10924',0,      'smte',0
	db '10922',0,      'smt',0
	db '10921',0,      'gescc',0
	db '10923',0,      'lat',0
	db '10924,65024',0,'smtes',0
	db '10926',0,      'bumpE',0
	db '10925,65024',0,'lates',0
	db '10925',0,      'late',0
	db '10927,824',0,  'npre',0
	db '10927',0,      'pre',0
	db '10934',0,      'scnE',0
	db '10931',0,      'prE',0
	db '10928',0,      'sce',0
	db '1093',0,       'khcy',0
	db '10932',0,      'scE',0
	db '10933',0,      'prnE',0
	db '10937',0,      'prnap',0
	db '10935',0,      'prap',0
	db '10936',0,      'scap',0
	db '10938',0,      'scnap',0
	db '10939',0,      'Pr',0
	db '1095',0,       'chcy',0
	db '10945',0,      'submult',0
	db '10942',0,      'supdot',0
	db '10940',0,      'Sc',0
	db '10941',0,      'subdot',0
	db '10943',0,      'subplus',0
	db '10944',0,      'supplus',0
	db '10948',0,      'supedot',0
	db '10946',0,      'supmult',0
	db '10947',0,      'subedot',0
	db '10949,824',0,  'nsubE',0
	db '10949',0,      'subE',0
	db '10955',0,      'subnE',0
	db '10951',0,      'subsim',0
	db '10950,824',0,  'nsupE',0
	db '10950',0,      'supE',0
	db '10952',0,      'supsim',0
	db '10955,65024',0,'vsubnE',0
	db '10959',0,      'csub',0
	db '10956,65024',0,'vsupnE',0
	db '10956',0,      'supnE',0
	db '1096',0,       'shcy',0
	db '10960',0,      'csup',0
	db '10991',0,      'cirmid',0
	db '10971',0,      'mlcp',0
	db '10966',0,      'supsup',0
	db '10963',0,      'subsup',0
	db '10962',0,      'csupe',0
	db '10964',0,      'supsub',0
	db '10965',0,      'subsub',0
	db '10969',0,      'forkv',0
	db '10967',0,      'suphsub',0
	db '10968',0,      'supdsub',0
	db '1097',0,       'shchcy',0
	db '10970',0,      'topfork',0
	db '10985',0,      'vBarv',0
	db '10982',0,      'Vdashl',0
	db '1098',0,       'hardcy',0
	db '10980',0,      'Dashv',0
	db '10983',0,      'Barv',0
	db '10984',0,      'vBar',0
	db '10989',0,      'bNot',0
	db '10987',0,      'Vbar',0
	db '10988',0,      'Not',0
	db '1099',0,       'ycy',0
	db '10990',0,      'rnmid',0
	db '1106',0,       'djcy',0
	db '11005,8421',0, 'nparsl',0
	db '10994',0,      'nhpar',0
	db '10992',0,      'midcir',0
	db '10993',0,      'topcir',0
	db '10995',0,      'parsim',0
	db '1100',0,       'softcy',0
	db '1102',0,       'yucy',0
	db '11005',0,      'parsl',0
	db '1101',0,       'ecy',0
	db '1103',0,       'yacy',0
	db '1105',0,       'iocy',0
	db '1112',0,       'jsercy',0
	db '1109',0,       'dscy',0
	db '1107',0,       'gjcy',0
	db '1108',0,       'jukcy',0
	db '1110',0,       'iukcy',0
	db '1111',0,       'yicy',0
	db '1115',0,       'tshcy',0
	db '1113',0,       'ljcy',0
	db '1114',0,       'njcy',0
	db '1116',0,       'kjcy',0
	db '1118',0,       'ubrcy',0
	db '120074',0,     'Gfr',0
	db '119995',0,     'fscr',0
	db '119982',0,     'Sscr',0
	db '119973',0,     'Jscr',0
	db '119966',0,     'Cscr',0
	db '119964',0,     'Ascr',0
	db '119967',0,     'Dscr',0
	db '119970',0,     'Gscr',0
	db '119978',0,     'Oscr',0
	db '119974',0,     'Kscr',0
	db '119977',0,     'Nscr',0
	db '119979',0,     'Pscr',0
	db '119980',0,     'Qscr',0
	db '119988',0,     'Yscr',0
	db '119985',0,     'Vscr',0
	db '119983',0,     'Tscr',0
	db '119984',0,     'Uscr',0
	db '119986',0,     'Wscr',0
	db '119987',0,     'Xscr',0
	db '119991',0,     'bscr',0
	db '119989',0,     'Zscr',0
	db '119990',0,     'ascr',0
	db '119992',0,     'cscr',0
	db '119993',0,     'dscr',0
	db '120009',0,     'tscr',0
	db '120002',0,     'mscr',0
	db '119999',0,     'jscr',0
	db '119997',0,     'hscr',0
	db '119998',0,     'iscr',0
	db '120000',0,     'kscr',0
	db '120001',0,     'lscr',0
	db '120006',0,     'qscr',0
	db '120003',0,     'nscr',0
	db '120005',0,     'pscr',0
	db '120007',0,     'rscr',0
	db '120008',0,     'sscr',0
	db '120015',0,     'zscr',0
	db '120012',0,     'wscr',0
	db '120010',0,     'uscr',0
	db '120011',0,     'vscr',0
	db '120013',0,     'xscr',0
	db '120014',0,     'yscr',0
	db '120071',0,     'Dfr',0
	db '120068',0,     'Afr',0
	db '120069',0,     'Bfr',0
	db '120072',0,     'Efr',0
	db '120073',0,     'Ffr',0
	db '120102',0,     'ifr',0
	db '120089',0,     'Vfr',0
	db '120082',0,     'Ofr',0
	db '120079',0,     'Lfr',0
	db '120077',0,     'Jfr',0
	db '120078',0,     'Kfr',0
	db '120080',0,     'Mfr',0
	db '120081',0,     'Nfr',0
	db '120086',0,     'Sfr',0
	db '120083',0,     'Pfr',0
	db '120084',0,     'Qfr',0
	db '120087',0,     'Tfr',0
	db '120088',0,     'Ufr',0
	db '120096',0,     'cfr',0
	db '120092',0,     'Yfr',0
	db '120090',0,     'Wfr',0
	db '120091',0,     'Xfr',0
	db '120094',0,     'afr',0
	db '120095',0,     'bfr',0
	db '120099',0,     'ffr',0
	db '120097',0,     'dfr',0
	db '120098',0,     'efr',0
	db '120100',0,     'gfr',0
	db '120101',0,     'hfr',0
	db '120114',0,     'ufr',0
	db '120108',0,     'ofr',0
	db '120105',0,     'lfr',0
	db '120103',0,     'jfr',0
	db '120104',0,     'kfr',0
	db '120106',0,     'mfr',0
	db '120107',0,     'nfr',0
	db '120111',0,     'rfr',0
	db '120109',0,     'pfr',0
	db '120110',0,     'qfr',0
	db '120112',0,     'sfr',0
	db '120113',0,     'tfr',0
	db '120120',0,     'Aopf',0
	db '120117',0,     'xfr',0
	db '120115',0,     'vfr',0
	db '120116',0,     'wfr',0
	db '120118',0,     'yfr',0
	db '120119',0,     'zfr',0
	db '120124',0,     'Eopf',0
	db '120121',0,     'Bopf',0
	db '120123',0,     'Dopf',0
	db '120125',0,     'Fopf',0
	db '120126',0,     'Gopf',0
	db '206',0,        'Icirc',0
	db '164',0,        'curren',0
	db '120156',0,     'kopf',0
	db '120143',0,     'Xopf',0
	db '120134',0,     'Oopf',0
	db '120130',0,     'Kopf',0
	db '120129',0,     'Jopf',0
	db '120131',0,     'Lopf',0
	db '120132',0,     'Mopf',0
	db '120140',0,     'Uopf',0
	db '120138',0,     'Sopf',0
	db '120139',0,     'Topf',0
	db '120141',0,     'Vopf',0
	db '120142',0,     'Wopf',0
	db '120150',0,     'eopf',0
	db '120147',0,     'bopf',0
	db '120144',0,     'Yopf',0
	db '120146',0,     'aopf',0
	db '120148',0,     'copf',0
	db '120149',0,     'dopf',0
	db '120153',0,     'hopf',0
	db '120151',0,     'fopf',0
	db '120152',0,     'gopf',0
	db '120154',0,     'iopf',0
	db '120155',0,     'jopf',0
	db '120168',0,     'wopf',0
	db '120162',0,     'qopf',0
	db '120159',0,     'nopf',0
	db '120157',0,     'lopf',0
	db '120158',0,     'mopf',0
	db '120160',0,     'oopf',0
	db '120161',0,     'popf',0
	db '120165',0,     'topf',0
	db '120163',0,     'ropf',0
	db '120164',0,     'sopf',0
	db '120166',0,     'uopf',0
	db '120167',0,     'vopf',0
	db '123',0,        'lbrace',0
	db '120171',0,     'zopf',0
	db '120169',0,     'xopf',0
	db '120170',0,     'yopf',0
	db '12296',0,      'lang',0
	db '12297',0,      'rang',0
	db '161',0,        'iexcl',0
	db '124',0,        'vert',0
	db '125',0,        'rbrace',0
	db '162',0,        'cent',0
	db '163',0,        'pound',0
	db '183',0,        'centerdot',0
	db '174',0,        'reg',0
	db '168',0,        'uml',0
	db '166',0,        'brvbar',0
	db '165',0,        'yen',0
	db '166',0,        'brvbar',0
	db '167',0,        'sect',0
	db '171',0,        'laquo',0
	db '169',0,        'copy',0
	db '170',0,        'ordf',0
	db '172',0,        'not',0
	db '173',0,        'shy',0
	db '178',0,        'sup2',0
	db '175',0,        'strns',0
	db '174',0,        'reg',0
	db '175',0,        'macr',0
	db '176',0,        'deg',0
	db '177',0,        'plusmn',0
	db '180',0,        'acute',0
	db '179',0,        'sup3',0
	db '180',0,        'acute',0
	db '181',0,        'micro',0
	db '182',0,        'para',0
	db '194',0,        'Acirc',0
	db '188',0,        'frac14',0
	db '185',0,        'sup1',0
	db '184',0,        'cedil',0
	db '184',0,        'cedil',0
	db '186',0,        'ordm',0
	db '187',0,        'raquo',0
	db '191',0,        'iquest',0
	db '189',0,        'frac12',0
	db '190',0,        'frac34',0
	db '192',0,        'Agrave',0
	db '193',0,        'Aacute',0
	db '200',0,        'Egrave',0
	db '197',0,        'Aring',0
	db '195',0,        'Atilde',0
	db '196',0,        'Auml',0
	db '198',0,        'AElig',0
	db '199',0,        'Ccedil',0
	db '203',0,        'Euml',0
	db '201',0,        'Eacute',0
	db '202',0,        'Ecirc',0
	db '204',0,        'Igrave',0
	db '205',0,        'Iacute',0
	db '253',0,        'yacute',0
	db '229',0,        'aring',0
	db '217',0,        'Ugrave',0
	db '211',0,        'Oacute',0
	db '208',0,        'ETH',0
	db '207',0,        'Iuml',0
	db '209',0,        'Ntilde',0
	db '210',0,        'Ograve',0
	db '214',0,        'Ouml',0
	db '212',0,        'Ocirc',0
	db '213',0,        'Otilde',0
	db '215',0,        'times',0
	db '216',0,        'Oslash',0
	db '223',0,        'szlig',0
	db '220',0,        'Uuml',0
	db '218',0,        'Uacute',0
	db '219',0,        'Ucirc',0
	db '221',0,        'Yacute',0
	db '222',0,        'THORN',0
	db '226',0,        'acirc',0
	db '224',0,        'agrave',0
	db '225',0,        'aacute',0
	db '227',0,        'atilde',0
	db '228',0,        'auml',0
	db '241',0,        'ntilde',0
	db '235',0,        'euml',0
	db '232',0,        'egrave',0
	db '230',0,        'aelig',0
	db '231',0,        'ccedil',0
	db '233',0,        'eacute',0
	db '234',0,        'ecirc',0
	db '238',0,        'icirc',0
	db '236',0,        'igrave',0
	db '237',0,        'iacute',0
	db '239',0,        'iuml',0
	db '240',0,        'eth',0
	db '247',0,        'divide',0
	db '244',0,        'ocirc',0
	db '242',0,        'ograve',0
	db '243',0,        'oacute',0
	db '245',0,        'otilde',0
	db '246',0,        'ouml',0
	db '250',0,        'uacute',0
	db '248',0,        'oslash',0
	db '249',0,        'ugrave',0
	db '251',0,        'ucirc',0
	db '252',0,        'uuml',0
	db '279',0,        'edot',0
	db '265',0,        'ccirc',0
	db '259',0,        'abreve',0
	db '256',0,        'Amacr',0
	db '254',0,        'thorn',0
	db '255',0,        'yuml',0
	db '257',0,        'amacr',0
	db '258',0,        'Abreve',0
	db '262',0,        'Cacute',0
	db '260',0,        'Aogon',0
	db '261',0,        'aogon',0
	db '263',0,        'cacute',0
	db '264',0,        'Ccirc',0
	db '271',0,        'dcaron',0
	db '268',0,        'Ccaron',0
	db '266',0,        'Cdot',0
	db '267',0,        'cdot',0
	db '269',0,        'ccaron',0
	db '270',0,        'Dcaron',0
	db '274',0,        'Emacr',0
	db '272',0,        'Dstrok',0
	db '273',0,        'dstrok',0
	db '275',0,        'emacr',0
	db '278',0,        'Edot',0
	db '292',0,        'Hcirc',0
	db '285',0,        'gcirc',0
	db '282',0,        'Ecaron',0
	db '280',0,        'Eogon',0
	db '281',0,        'eogon',0
	db '283',0,        'ecaron',0
	db '284',0,        'Gcirc',0
	db '288',0,        'Gdot',0
	db '286',0,        'Gbreve',0
	db '287',0,        'gbreve',0
	db '289',0,        'gdot',0
	db '290',0,        'Gcedil',0
	db '298',0,        'Imacr',0
	db '295',0,        'hstrok',0
	db '293',0,        'hcirc',0
	db '294',0,        'Hstrok',0
	db '296',0,        'Itilde',0
	db '297',0,        'itilde',0
	db '303',0,        'iogon',0
	db '299',0,        'imacr',0
	db '302',0,        'Iogon',0
	db '304',0,        'Idot',0
	db '305',0,        'inodot',0
	db '8783,824',0,   'nbumpe',0
	db '8495',0,       'escr',0
	db '567',0,        'jmath',0
	db '351',0,        'scedil',0
	db '328',0,        'ncaron',0
	db '317',0,        'Lcaron',0
	db '311',0,        'kcedil',0
	db '308',0,        'Jcirc',0
	db '307',0,        'ijlig',0
	db '309',0,        'jcirc',0
	db '310',0,        'Kcedil',0
	db '314',0,        'lacute',0
	db '312',0,        'kgreen',0
	db '313',0,        'Lacute',0
	db '315',0,        'Lcedil',0
	db '316',0,        'lcedil',0
	db '322',0,        'lstrok',0
	db '32',0,         'nbsp',0
	db '318',0,        'lcaron',0
	db '319',0,        'Lmidot',0
	db '320',0,        'lmidot',0
	db '321',0,        'Lstrok',0
	db '325',0,        'Ncedil',0
	db '323',0,        'Nacute',0
	db '324',0,        'nacute',0
	db '326',0,        'ncedil',0
	db '327',0,        'Ncaron',0
	db '340',0,        'Racute',0
	db '333',0,        'omacr',0
	db '330',0,        'ENG',0
	db '329',0,        'napos',0
	db '33',0,         'excl',0
	db '331',0,        'eng',0
	db '332',0,        'Omacr',0
	db '338',0,        'OElig',0
	db '336',0,        'Odblac',0
	db '337',0,        'odblac',0
	db '339',0,        'oelig',0
	db '34',0,         'quot',0
	db '346',0,        'Sacute',0
	db '343',0,        'rcedil',0
	db '341',0,        'racute',0
	db '342',0,        'Rcedil',0
	db '344',0,        'Rcaron',0
	db '345',0,        'rcaron',0
	db '349',0,        'scirc',0
	db '347',0,        'sacute',0
	db '348',0,        'Scirc',0
	db '35',0,         'num',0
	db '350',0,        'Scedil',0
	db '372',0,        'Wcirc',0
	db '361',0,        'utilde',0
	db '356',0,        'Tcaron',0
	db '353',0,        'scaron',0
	db '352',0,        'Scaron',0
	db '354',0,        'Tcedil',0
	db '355',0,        'tcedil',0
	db '359',0,        'tstrok',0
	db '357',0,        'tcaron',0
	db '358',0,        'Tstrok',0
	db '36',0,         'dollar',0
	db '360',0,        'Utilde',0
	db '367',0,        'uring',0
	db '364',0,        'Ubreve',0
	db '362',0,        'Umacr',0
	db '363',0,        'umacr',0
	db '365',0,        'ubreve',0
	db '366',0,        'Uring',0
	db '37',0,         'percnt',0
	db '368',0,        'Udblac',0
	db '369',0,        'udblac',0
	db '370',0,        'Uogon',0
	db '371',0,        'uogon',0
	db '39',0,         'apos',0
	db '378',0,        'zacute',0
	db '375',0,        'ycirc',0
	db '373',0,        'wcirc',0
	db '374',0,        'Ycirc',0
	db '376',0,        'Yuml',0
	db '377',0,        'Zacute',0
	db '380',0,        'zdot',0
	db '379',0,        'Zdot',0
	db '38',0,         'amp',0
	db '381',0,        'Zcaron',0
	db '382',0,        'zcaron',0
	db '437',0,        'imped',0
	db '41',0,         'rpar',0
	db '40',0,         'lpar',0
	db '402',0,        'fnof',0
	db '42',0,         'ast',0
	db '43',0,         'plus',0
	db '46',0,         'period',0
	db '44',0,         'comma',0
	db '45',0,         'minus',0
	db '47',0,         'sol',0
	db '501',0,        'gacute',0
	db '8242',0,       'prime',0
	db '731',0,        'ogon',0
	db '63',0,         'quest',0
	db '60',0,         'lt',0
	db '59',0,         'semi',0
	db '58',0,         'colon',0
	db '60,8402',0,    'nvlt',0
	db '60',0,         'LT',0
	db '62,8402',0,    'nvgt',0
	db '61,8421',0,    'bne',0
	db '61',0,         'equals',0
	db '62',0,         'GT',0
	db '62',0,         'gt',0
	db '64260',0,      'ffllig',0
	db '64257',0,      'filig',0
	db '64',0,         'commat',0
	db '64256',0,      'fflig',0
	db '64258',0,      'fllig',0
	db '64259',0,      'ffilig',0
	db '728',0,        'breve',0
	db '710',0,        'circ',0
	db '711',0,        'caron',0
	db '729',0,        'dot',0
	db '730',0,        'ring',0
	db '8217',0,       'rsquo',0
	db '8208',0,       'dash',0
	db '785',0,        'DownBreve',0
	db '732',0,        'tilde',0
	db '733',0,        'dblac',0
	db '8204',0,       'zwnj',0
	db '8205',0,       'zwj',0
	db '8213',0,       'horbar',0
	db '8211',0,       'ndash',0
	db '8212',0,       'mdash',0
	db '8214',0,       'Vert',0
	db '8216',0,       'lsquo',0
	db '8225',0,       'Dagger',0
	db '8221',0,       'rdquo',0
	db '8218',0,       'sbquo',0
	db '8220',0,       'ldquo',0
	db '8222',0,       'bdquo',0
	db '8224',0,       'dagger',0
	db '8230',0,       'mldr',0
	db '8226',0,       'bull',0
	db '8229',0,       'nldr',0
	db '8240',0,       'permil',0
	db '8241',0,       'pertenk',0
	db '8461',0,       'Hopf',0
	db '8288',0,       'NoBreak',0
	db '8254',0,       'oline',0
	db '8245',0,       'bprime',0
	db '8243',0,       'Prime',0
	db '8244',0,       'tprime',0
	db '8249',0,       'lsaquo',0
	db '8250',0,       'rsaquo',0
	db '8260',0,       'frasl',0
	db '8257',0,       'caret',0
	db '8259',0,       'hybull',0
	db '8271',0,       'bsemi',0
	db '8279',0,       'qprime',0
	db '8412',0,       'DotDot',0
	db '8291',0,       'ic',0
	db '8289',0,       'af',0
	db '8290',0,       'it',0
	db '8364',0,       'euro',0
	db '8411',0,       'tdot',0
	db '8458',0,       'gscr',0
	db '8450',0,       'Copf',0
	db '8453',0,       'incare',0
	db '8459',0,       'Hscr',0
	db '8460',0,       'Hfr',0
	db '8474',0,       'Qopf',0
	db '8467',0,       'ell',0
	db '8464',0,       'Iscr',0
	db '8462',0,       'planckh',0
	db '8463',0,       'hbar',0
	db '8465',0,       'Ifr',0
	db '8466',0,       'Lscr',0
	db '8471',0,       'copysr',0
	db '8469',0,       'Nopf',0
	db '8470',0,       'numero',0
	db '8472',0,       'wp',0
	db '8473',0,       'Popf',0
	db '8484',0,       'Zopf',0
	db '8477',0,       'Ropf',0
	db '8475',0,       'Rscr',0
	db '8476',0,       'Rfr',0
	db '8478',0,       'rx',0
	db '8482',0,       'trade',0
	db '8489',0,       'iiota',0
	db '8487',0,       'mho',0
	db '8488',0,       'Zfr',0
	db '8492',0,       'Bscr',0
	db '8493',0,       'Cfr',0
	db '8661',0,       'vArr',0
	db '8613',0,       'mapstoup',0
	db '8541',0,       'frac58',0
	db '8519',0,       'ee',0
	db '8501',0,       'aleph',0
	db '8497',0,       'Fscr',0
	db '8496',0,       'Escr',0
	db '8499',0,       'Mscr',0
	db '8500',0,       'oscr',0
	db '8504',0,       'daleth',0
	db '8502',0,       'beth',0
	db '8503',0,       'gimel',0
	db '8517',0,       'DD',0
	db '8518',0,       'dd',0
	db '8535',0,       'frac35',0
	db '8532',0,       'frac23',0
	db '8520',0,       'ii',0
	db '8531',0,       'frac13',0
	db '8533',0,       'frac15',0
	db '8534',0,       'frac25',0
	db '8538',0,       'frac56',0
	db '8536',0,       'frac45',0
	db '8537',0,       'frac16',0
	db '8539',0,       'frac18',0
	db '8540',0,       'frac38',0
	db '8602',0,       'nlarr',0
	db '8596',0,       'harr',0
	db '8593',0,       'uarr',0
	db '8542',0,       'frac78',0
	db '8592',0,       'larr',0
	db '8594',0,       'rarr',0
	db '8595',0,       'darr',0
	db '8599',0,       'nearr',0
	db '8597',0,       'varr',0
	db '8598',0,       'nwarr',0
	db '8600',0,       'searr',0
	db '8601',0,       'swarr',0
	db '8608',0,       'Rarr',0
	db '8605',0,       'rarrw',0
	db '8603',0,       'nrarr',0
	db '8605,824',0,   'nrarrw',0
	db '8606',0,       'Larr',0
	db '8607',0,       'Uarr',0
	db '8611',0,       'rarrtl',0
	db '8609',0,       'Darr',0
	db '8610',0,       'larrtl',0
	db '8612',0,       'LeftTeeArrow',0
	db '8613',0,       'UpTeeArrow',0
	db '8640',0,       'RightVector',0
	db '8626',0,       'ldsh',0
	db '8619',0,       'larrlp',0
	db '8615',0,       'DownTeeArrow',0
	db '8614',0,       'RightTeeArrow',0
	db '8617',0,       'larrhk',0
	db '8618',0,       'rarrhk',0
	db '8622',0,       'nharr',0
	db '8620',0,       'rarrlp',0
	db '8621',0,       'harrw',0
	db '8624',0,       'Lsh',0
	db '8625',0,       'rsh',0
	db '8635',0,       'orarr',0
	db '8630',0,       'cularr',0
	db '8627',0,       'rdsh',0
	db '8629',0,       'crarr',0
	db '8631',0,       'curarr',0
	db '8634',0,       'olarr',0
	db '8638',0,       'RightUpVector',0
	db '8636',0,       'lharu',0
	db '8637',0,       'lhard',0
	db '8638',0,       'uharr',0
	db '8639',0,       'uharl',0
	db '8649',0,       'rrarr',0
	db '8643',0,       'dharl',0
	db '8642',0,       'dharr',0
	db '8640',0,       'rharu',0
	db '8641',0,       'rhard',0
	db '8642',0,       'dharr',0
	db '8643',0,       'dharl',0
	db '8646',0,       'lrarr',0
	db '8644',0,       'rlarr',0
	db '8645',0,       'udarr',0
	db '8647',0,       'llarr',0
	db '8648',0,       'uuarr',0
	db '8655',0,       'nrArr',0
	db '8652',0,       'rlhar',0
	db '8650',0,       'ddarr',0
	db '8651',0,       'lrhar',0
	db '8653',0,       'nlArr',0
	db '8654',0,       'nhArr',0
	db '8658',0,       'rArr',0
	db '8656',0,       'lArr',0
	db '8657',0,       'uArr',0
	db '8659',0,       'dArr',0
	db '8660',0,       'hArr',0
	db '8743',0,       'and',0
	db '8713',0,       'notin',0
	db '8701',0,       'loarr',0
	db '8666',0,       'lAarr',0
	db '8663',0,       'neArr',0
	db '8662',0,       'nwArr',0
	db '8664',0,       'seArr',0
	db '8665',0,       'swArr',0
	db '8676',0,       'larrb',0
	db '8667',0,       'rAarr',0
	db '8669',0,       'zigrarr',0
	db '8677',0,       'rarrb',0
	db '8693',0,       'duarr',0
	db '8706',0,       'part',0
	db '8704',0,       'forall',0
	db '8702',0,       'roarr',0
	db '8703',0,       'hoarr',0
	db '8705',0,       'comp',0
	db '8706,824',0,   'npart',0
	db '8709',0,       'empty',0
	db '8707',0,       'exist',0
	db '8708',0,       'nexist',0
	db '8711',0,       'Del',0
	db '8712',0,       'in',0
	db '8730',0,       'radic',0
	db '8722',0,       'minus',0
	db '8719',0,       'prod',0
	db '8715',0,       'ni',0
	db '8716',0,       'notni',0
	db '8720',0,       'coprod',0
	db '8721',0,       'Sum',0
	db '8726',0,       'setmn',0
	db '8723',0,       'mp',0
	db '8724',0,       'plusdo',0
	db '8727',0,       'lowast',0
	db '8728',0,       'compfn',0
	db '8737',0,       'angmsd',0
	db '8735',0,       'angrt',0
	db '8733',0,       'prop',0
	db '8734',0,       'infin',0
	db '8736,8402',0,  'nang',0
	db '8736',0,       'ang',0
	db '8740',0,       'nmid',0
	db '8738',0,       'angsph',0
	db '8739',0,       'mid',0
	db '8741',0,       'par',0
	db '8742',0,       'npar',0
	db '8765,817',0,   'race',0
	db '8753',0,       'cwint',0
	db '8747',0,       'int',0
	db '8745',0,       'cap',0
	db '8744',0,       'or',0
	db '8745,65024',0, 'caps',0
	db '8746,65024',0, 'cups',0
	db '8746',0,       'cup',0
	db '8750',0,       'oint',0
	db '8748',0,       'Int',0
	db '8749',0,       'tint',0
	db '8751',0,       'Conint',0
	db '8752',0,       'Cconint',0
	db '8759',0,       'Colon',0
	db '8756',0,       'there4',0
	db '8754',0,       'cwconint',0
	db '8755',0,       'awconint',0
	db '8757',0,       'becaus',0
	db '8758',0,       'ratio',0
	db '8763',0,       'homtht',0
	db '8760',0,       'minusd',0
	db '8762',0,       'mDDot',0
	db '8764,8402',0,  'nvsim',0
	db '8764',0,       'sim',0
	db '8774',0,       'simne',0
	db '8769',0,       'nsim',0
	db '8766',0,       'ac',0
	db '8765',0,       'bsim',0
	db '8766,819',0,   'acE',0
	db '8767',0,       'acd',0
	db '8768',0,       'wr',0
	db '8771',0,       'sime',0
	db '8770,824',0,   'nesim',0
	db '8770',0,       'esim',0
	db '8772',0,       'nsime',0
	db '8773',0,       'cong',0
	db '8779',0,       'apid',0
	db '8777',0,       'nap',0
	db '8775',0,       'ncong',0
	db '8776',0,       'asymp',0
	db '8778',0,       'ape',0
	db '8779,824',0,   'napid',0
	db '8781',0,       'CupCap',0
	db '8780',0,       'bcong',0
	db '8781,8402',0,  'nvap',0
	db '8782,824',0,   'nbump',0
	db '8782',0,       'bump',0
	db '8957',0,       'notnivc',0
	db '8862',0,       'plusb',0
	db '8822',0,       'lg',0
	db '8806,824',0,   'nlE',0
	db '8793',0,       'wedgeq',0
	db '8786',0,       'efDot',0
	db '8784,824',0,   'nedot',0
	db '8783',0,       'bumpe',0
	db '8784',0,       'esdot',0
	db '8785',0,       'eDot',0
	db '8789',0,       'ecolon',0
	db '8787',0,       'erDot',0
	db '8788',0,       'colone',0
	db '8790',0,       'ecir',0
	db '8791',0,       'cire',0
	db '8801',0,       'equiv',0
	db '8799',0,       'equest',0
	db '8794',0,       'veeeq',0
	db '8796',0,       'trie',0
	db '8800',0,       'ne',0
	db '8801,8421',0,  'bnequiv',0
	db '8804',0,       'le',0
	db '8802',0,       'nequiv',0
	db '8804,8402',0,  'nvle',0
	db '8805,8402',0,  'nvge',0
	db '8805',0,       'ge',0
	db '8811,8402',0,  'nGt',0
	db '8809,65024',0, 'gvnE',0
	db '8807',0,       'gE',0
	db '8806',0,       'lE',0
	db '8807,824',0,   'ngE',0
	db '8808,65024',0, 'lvnE',0
	db '8808',0,       'lnE',0
	db '8810,8402',0,  'nLt',0
	db '8809',0,       'gnE',0
	db '8810,824',0,   'nLtv',0
	db '8810',0,       'll',0
	db '8811,824',0,   'nGtv',0
	db '8816',0,       'nle',0
	db '8813',0,       'NotCupCap',0
	db '8811',0,       'gg',0
	db '8812',0,       'twixt',0
	db '8814',0,       'nlt',0
	db '8815',0,       'ngt',0
	db '8819',0,       'gsim',0
	db '8817',0,       'nge',0
	db '8818',0,       'lsim',0
	db '8820',0,       'nlsim',0
	db '8821',0,       'ngsim',0
	db '8842,65024',0, 'vsubne',0
	db '8832',0,       'npr',0
	db '8827',0,       'sc',0
	db '8824',0,       'ntlg',0
	db '8823',0,       'gl',0
	db '8825',0,       'ntgl',0
	db '8826',0,       'pr',0
	db '8830',0,       'prsim',0
	db '8828',0,       'prcue',0
	db '8829',0,       'sccue',0
	db '8831,824',0,   'NotSucceedsTilde',0
	db '8831',0,       'scsim',0
	db '8836',0,       'nsub',0
	db '8834',0,       'sub',0
	db '8833',0,       'nsc',0
	db '8834,8402',0,  'vnsub',0
	db '8835,8402',0,  'vnsup',0
	db '8835',0,       'sup',0
	db '8839',0,       'supe',0
	db '8837',0,       'nsup',0
	db '8838',0,       'sube',0
	db '8840',0,       'nsube',0
	db '8841',0,       'nsupe',0
	db '8851,65024',0, 'sqcaps',0
	db '8847,824',0,   'NotSquareSubset',0
	db '8843',0,       'supne',0
	db '8842',0,       'subne',0
	db '8843,65024',0, 'vsupne',0
	db '8845',0,       'cupdot',0
	db '8846',0,       'uplus',0
	db '8848',0,       'sqsup',0
	db '8847',0,       'sqsub',0
	db '8848,824',0,   'NotSquareSuperset',0
	db '8849',0,       'sqsube',0
	db '8850',0,       'sqsupe',0
	db '8855',0,       'otimes',0
	db '8852',0,       'sqcup',0
	db '8851',0,       'sqcap',0
	db '8852,65024',0, 'sqcups',0
	db '8853',0,       'oplus',0
	db '8854',0,       'ominus',0
	db '8858',0,       'ocir',0
	db '8856',0,       'osol',0
	db '8857',0,       'odot',0
	db '8859',0,       'oast',0
	db '8861',0,       'odash',0
	db '8910',0,       'cuvee',0
	db '8885',0,       'rtrie',0
	db '8874',0,       'Vvdash',0
	db '8867',0,       'dashv',0
	db '8864',0,       'timesb',0
	db '8863',0,       'minusb',0
	db '8865',0,       'sdotb',0
	db '8866',0,       'vdash',0
	db '8871',0,       'models',0
	db '8868',0,       'top',0
	db '8869',0,       'bot',0
	db '8872',0,       'vDash',0
	db '8873',0,       'Vdash',0
	db '8880',0,       'prurel',0
	db '8877',0,       'nvDash',0
	db '8875',0,       'VDash',0
	db '8876',0,       'nvdash',0
	db '8878',0,       'nVdash',0
	db '8879',0,       'nVDash',0
	db '8884,8402',0,  'nvltrie',0
	db '8882',0,       'vltri',0
	db '8883',0,       'vrtri',0
	db '8884',0,       'ltrie',0
	db '8885,8402',0,  'nvrtrie',0
	db '8898',0,       'xcap',0
	db '8891',0,       'veebar',0
	db '8888',0,       'mumap',0
	db '8886',0,       'origof',0
	db '8887',0,       'imof',0
	db '8889',0,       'hercon',0
	db '8890',0,       'intcal',0
	db '8895',0,       'lrtri',0
	db '8893',0,       'barvee',0
	db '8894',0,       'angrtvb',0
	db '8896',0,       'Wedge',0
	db '8897',0,       'Vee',0
	db '8904',0,       'bowtie',0
	db '8901',0,       'sdot',0
	db '8899',0,       'xcup',0
	db '8900',0,       'diam',0
	db '8902',0,       'Star',0
	db '8903',0,       'divonx',0
	db '8907',0,       'lthree',0
	db '8905',0,       'ltimes',0
	db '8906',0,       'rtimes',0
	db '8908',0,       'rthree',0
	db '8909',0,       'bsime',0
	db '8934',0,       'lnsim',0
	db '8921,824',0,   'nGg',0
	db '8916',0,       'fork',0
	db '8913',0,       'Sup',0
	db '8911',0,       'cuwed',0
	db '8912',0,       'Sub',0
	db '8914',0,       'Cap',0
	db '8915',0,       'Cup',0
	db '8919',0,       'gtdot',0
	db '8917',0,       'epar',0
	db '8918',0,       'ltdot',0
	db '8920,824',0,   'nLl',0
	db '8920',0,       'Ll',0
	db '8926',0,       'cuepr',0
	db '8922',0,       'leg',0
	db '8921',0,       'Gg',0
	db '8922,65024',0, 'lesg',0
	db '8923,65024',0, 'gesl',0
	db '8923',0,       'gel',0
	db '8929',0,       'nsccue',0
	db '8927',0,       'cuesc',0
	db '8928',0,       'nprcue',0
	db '8930',0,       'nsqsube',0
	db '8931',0,       'nsqsupe',0
	db '8946',0,       'disin',0
	db '8940',0,       'nltrie',0
	db '8937',0,       'scnsim',0
	db '8935',0,       'gnsim',0
	db '8936',0,       'prnsim',0
	db '8938',0,       'nltri',0
	db '8939',0,       'nrtri',0
	db '8943',0,       'ctdot',0
	db '8941',0,       'nrtrie',0
	db '8942',0,       'vellip',0
	db '8944',0,       'utdot',0
	db '8945',0,       'dtdot',0
	db '8951',0,       'notinvb',0
	db '8949,824',0,   'notindot',0
	db '8947',0,       'isinsv',0
	db '8948',0,       'isins',0
	db '8949',0,       'isindot',0
	db '8950',0,       'notinvc',0
	db '8954',0,       'nisd',0
	db '8953,824',0,   'notinE',0
	db '8953',0,       'isinE',0
	db '8955',0,       'xnis',0
	db '8956',0,       'nis',0
	db '955',0,        'lambda',0
	db '9191',0,       'elinters',0
	db '9005',0,       'cylcty',0
	db '8975',0,       'ulcrop',0
	db '8969',0,       'rceil',0
	db '8965',0,       'barwed',0
	db '8958',0,       'notnivb',0
	db '8966',0,       'Barwed',0
	db '8968',0,       'lceil',0
	db '8972',0,       'drcrop',0
	db '8970',0,       'lfloor',0
	db '8971',0,       'rfloor',0
	db '8973',0,       'dlcrop',0
	db '8974',0,       'urcrop',0
	db '8988',0,       'ulcorn',0
	db '8979',0,       'profsurf',0
	db '8976',0,       'bnot',0
	db '8978',0,       'profline',0
	db '8981',0,       'telrec',0
	db '8982',0,       'target',0
	db '8991',0,       'drcorn',0
	db '8989',0,       'urcorn',0
	db '8990',0,       'dlcorn',0
	db '8994',0,       'frown',0
	db '8995',0,       'smile',0
	db '9141',0,       'bbrk',0
	db '91',0,         'lsqb',0
	db '9021',0,       'ovbar',0
	db '9006',0,       'profalar',0
	db '9014',0,       'topbot',0
	db '9023',0,       'solbar',0
	db '9084',0,       'angzarr',0
	db '9137',0,       'rmoust',0
	db '913',0,        'Alpha',0
	db '9136',0,       'lmoust',0
	db '914',0,        'Beta',0
	db '9140',0,       'tbrk',0
	db '9180',0,       'OverParenthesis',0
	db '916',0,        'Delta',0
	db '9142',0,       'bbrktbrk',0
	db '915',0,        'Gamma',0
	db '917',0,        'Epsilon',0
	db '918',0,        'Zeta',0
	db '9183',0,       'UnderBrace',0
	db '9181',0,       'UnderParenthesis',0
	db '9182',0,       'OverBrace',0
	db '9186',0,       'trpezium',0
	db '919',0,        'Eta',0
	db '9416',0,       'oS',0
	db '927',0,        'Omicron',0
	db '9226',0,       'NewLine',0
	db '921',0,        'Iota',0
	db '92',0,         'bsol',0
	db '920',0,        'Theta',0
	db '922',0,        'Kappa',0
	db '9225',0,       'Tab',0
	db '925',0,        'Nu',0
	db '923',0,        'Lambda',0
	db '924',0,        'Mu',0
	db '9251',0,       'blank',0
	db '926',0,        'Xi',0
	db '933',0,        'Upsilon',0
	db '93',0,         'rsqb',0
	db '928',0,        'Pi',0
	db '929',0,        'Rho',0
	db '931',0,        'Sigma',0
	db '932',0,        'Tau',0
	db '936',0,        'Psi',0
	db '934',0,        'Phi',0
	db '935',0,        'Chi',0
	db '937',0,        'Omega',0
	db '94',0,         'Hat',0
	db '9496',0,       'boxul',0
	db '948',0,        'delta',0
	db '947',0,        'gamma',0
	db '945',0,        'alpha',0
	db '946',0,        'beta',0
	db '9472',0,       'boxh',0
	db '9474',0,       'boxv',0
	db '949',0,        'epsilon',0
	db '9484',0,       'boxdr',0
	db '9488',0,       'boxdl',0
	db '949',0,        'epsilon',0
	db '9492',0,       'boxur',0
	db '9516',0,       'boxhd',0
	db '9500',0,       'boxvr',0
	db '95',0,         'lowbar',0
	db '950',0,        'zeta',0
	db '9508',0,       'boxvl',0
	db '951',0,        'eta',0
	db '953',0,        'iota',0
	db '952',0,        'theta',0
	db '9524',0,       'boxhu',0
	db '9532',0,       'boxvh',0
	db '954',0,        'kappa',0
	db '9642',0,       'squf',0
	db '9572',0,       'boxHd',0
	db '9561',0,       'boxUr',0
	db '9556',0,       'boxDR',0
	db '9553',0,       'boxV',0
	db '9552',0,       'boxH',0
	db '9554',0,       'boxdR',0
	db '9555',0,       'boxDr',0
	db '9559',0,       'boxDL',0
	db '9557',0,       'boxdL',0
	db '9558',0,       'boxDl',0
	db '956',0,        'mu',0
	db '9560',0,       'boxuR',0
	db '9567',0,       'boxVr',0
	db '9564',0,       'boxUl',0
	db '9562',0,       'boxUR',0
	db '9563',0,       'boxuL',0
	db '9565',0,       'boxUL',0
	db '9566',0,       'boxvR',0
	db '957',0,        'nu',0
	db '9568',0,       'boxVR',0
	db '9569',0,       'boxvL',0
	db '9570',0,       'boxVl',0
	db '9571',0,       'boxVL',0
	db '960',0,        'pi',0
	db '9578',0,       'boxvH',0
	db '9575',0,       'boxHu',0
	db '9573',0,       'boxhD',0
	db '9574',0,       'boxHD',0
	db '9576',0,       'boxhU',0
	db '9577',0,       'boxHU',0
	db '9580',0,       'boxVH',0
	db '9579',0,       'boxVh',0
	db '958',0,        'xi',0
	db '959',0,        'omicron',0
	db '96',0,         'grave',0
	db '9618',0,       'blk12',0
	db '9608',0,       'block',0
	db '9600',0,       'uhblk',0
	db '9604',0,       'lhblk',0
	db '961',0,        'rho',0
	db '9617',0,       'blk14',0
	db '963',0,        'sigma',0
	db '9619',0,       'blk34',0
	db '962',0,        'sigmaf',0
	db '9633',0,       'squ',0
	db '964',0,        'tau',0
	db '9720',0,       'ultri',0
	db '9661',0,       'xdtri',0
	db '9651',0,       'xutri',0
	db '9646',0,       'marker',0
	db '9643',0,       'EmptyVerySmallSquare',0
	db '9645',0,       'rect',0
	db '9649',0,       'fltns',0
	db '965',0,        'upsilon',0
	db '9656',0,       'rtrif',0
	db '9652',0,       'utrif',0
	db '9653',0,       'utri',0
	db '9657',0,       'rtri',0
	db '966',0,        'phi',0
	db '9674',0,       'loz',0
	db '9666',0,       'ltrif',0
	db '9662',0,       'dtrif',0
	db '9663',0,       'dtri',0
	db '9667',0,       'ltri',0
	db '967',0,        'chi',0
	db '969',0,        'omega',0
	db '9675',0,       'cir',0
	db '968',0,        'psi',0
	db '9708',0,       'tridot',0
	db '9711',0,       'xcirc',0
	db '981',0,        'straightphi',0
	db '9734',0,       'star',0
	db '9723',0,       'EmptySmallSquare',0
	db '9721',0,       'urtri',0
	db '9722',0,       'lltri',0
	db '9724',0,       'FilledSmallSquare',0
	db '9733',0,       'starf',0
	db '978',0,        'upsih',0
	db '9742',0,       'phone',0
	db '977',0,        'thetasym',0
	db '9792',0,       'female',0
	db '9794',0,       'male',0
	db '9834',0,       'sung',0
	db '9827',0,       'clubs',0
	db '982',0,        'piv',0
	db '9824',0,       'spades',0
	db '9829',0,       'hearts',0
	db '9830',0,       'diams',0
	db '9839',0,       'sharp',0
	db '9837',0,       'flat',0
	db '9838',0,       'natur',0
	db '988',0,        'Gammad',0
	db '989',0,        'gammad',0
	db 0
end;

{$ENDIF}

