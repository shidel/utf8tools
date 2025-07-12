{ HTML to UTF-8 conversion map }

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

{$DEFINE HTMLtoUTF8Remap}
const
  HTMLtoUTF8RemapList : TTextRemapEntries = (
    (Value:'gopf';                              Data:'120152'),
    (Value:'Uopf';                              Data:'120140'),
    (Value:'LessFullEqual';                     Data:'8806'),
    (Value:'EmptySmallSquare';                  Data:'9723'),
    (Value:'CupCap';                            Data:'8781'),
    (Value:'CHcy';                              Data:'1063'),
    (Value:'Ascr';                              Data:'119964'),
    (Value:'Agrave';                            Data:'192'),
    (Value:'Abreve';                            Data:'258'),
    (Value:'AMP';                               Data:'38'),
    (Value:'AElig';                             Data:'198'),
    (Value:'Aacute';                            Data:'193'),
    (Value:'Acy';                               Data:'1040'),
    (Value:'Acirc';                             Data:'194'),
    (Value:'Afr';                               Data:'120068'),
    (Value:'Aogon';                             Data:'260'),
    (Value:'Amacr';                             Data:'256'),
    (Value:'Alpha';                             Data:'913'),
    (Value:'And';                               Data:'10835'),
    (Value:'ApplyFunction';                     Data:'8289'),
    (Value:'Aopf';                              Data:'120120'),
    (Value:'Aring';                             Data:'197'),
    (Value:'Because';                           Data:'8757'),
    (Value:'Backslash';                         Data:'8726'),
    (Value:'Atilde';                            Data:'195'),
    (Value:'Assign';                            Data:'8788'),
    (Value:'Auml';                              Data:'196'),
    (Value:'Barwed';                            Data:'8966'),
    (Value:'Barv';                              Data:'10983'),
    (Value:'Bcy';                               Data:'1041'),
    (Value:'Bopf';                              Data:'120121'),
    (Value:'Beta';                              Data:'914'),
    (Value:'Bernoullis';                        Data:'8492'),
    (Value:'Bfr';                               Data:'120069'),
    (Value:'Bscr';                              Data:'8492'),
    (Value:'Breve';                             Data:'728'),
    (Value:'Bumpeq';                            Data:'8782'),
    (Value:'CircleMinus';                       Data:'8854'),
    (Value:'Ccirc';                             Data:'264'),
    (Value:'CapitalDifferentialD';              Data:'8517'),
    (Value:'Cacute';                            Data:'262'),
    (Value:'COPY';                              Data:'169'),
    (Value:'Cap';                               Data:'8914'),
    (Value:'Ccaron';                            Data:'268'),
    (Value:'Cayleys';                           Data:'8493'),
    (Value:'Ccedil';                            Data:'199'),
    (Value:'CenterDot';                         Data:'183'),
    (Value:'Cdot';                              Data:'266'),
    (Value:'Cconint';                           Data:'8752'),
    (Value:'Cedilla';                           Data:'184'),
    (Value:'Chi';                               Data:'935'),
    (Value:'Cfr';                               Data:'8493'),
    (Value:'CircleDot';                         Data:'8857'),
    (Value:'Congruent';                         Data:'8801'),
    (Value:'CloseCurlyDoubleQuote';             Data:'8221'),
    (Value:'CircleTimes';                       Data:'8855'),
    (Value:'CirclePlus';                        Data:'8853'),
    (Value:'ClockwiseContourIntegral';          Data:'8754'),
    (Value:'Colon';                             Data:'8759'),
    (Value:'CloseCurlyQuote';                   Data:'8217'),
    (Value:'Colone';                            Data:'10868'),
    (Value:'Coproduct';                         Data:'8720'),
    (Value:'ContourIntegral';                   Data:'8750'),
    (Value:'Conint';                            Data:'8751'),
    (Value:'Copf';                              Data:'8450'),
    (Value:'Cross';                             Data:'10799'),
    (Value:'CounterClockwiseContourIntegral';   Data:'8755'),
    (Value:'Cscr';                              Data:'119966'),
    (Value:'Cup';                               Data:'8915'),
    (Value:'DoubleLongRightArrow';              Data:'10233'),
    (Value:'DiacriticalDoubleAcute';            Data:'733'),
    (Value:'Dashv';                             Data:'10980'),
    (Value:'DScy';                              Data:'1029'),
    (Value:'DDotrahd';                          Data:'10513'),
    (Value:'DD';                                Data:'8517'),
    (Value:'DJcy';                              Data:'1026'),
    (Value:'Dagger';                            Data:'8225'),
    (Value:'DZcy';                              Data:'1039'),
    (Value:'Darr';                              Data:'8609'),
    (Value:'Delta';                             Data:'916'),
    (Value:'Dcy';                               Data:'1044'),
    (Value:'Dcaron';                            Data:'270'),
    (Value:'Del';                               Data:'8711'),
    (Value:'DiacriticalAcute';                  Data:'180'),
    (Value:'Dfr';                               Data:'120071'),
    (Value:'DiacriticalDot';                    Data:'729'),
    (Value:'DotEqual';                          Data:'8784'),
    (Value:'DifferentialD';                     Data:'8518'),
    (Value:'DiacriticalTilde';                  Data:'732'),
    (Value:'DiacriticalGrave';                  Data:'96'),
    (Value:'Diamond';                           Data:'8900'),
    (Value:'Dot';                               Data:'168'),
    (Value:'Dopf';                              Data:'120123'),
    (Value:'DotDot';                            Data:'8412'),
    (Value:'DoubleLeftArrow';                   Data:'8656'),
    (Value:'DoubleDot';                         Data:'168'),
    (Value:'DoubleContourIntegral';             Data:'8751'),
    (Value:'DoubleDownArrow';                   Data:'8659'),
    (Value:'DoubleLeftTee';                     Data:'10980'),
    (Value:'DoubleLeftRightArrow';              Data:'8660'),
    (Value:'DoubleLongLeftArrow';               Data:'10232'),
    (Value:'DoubleLongLeftRightArrow';          Data:'10234'),
    (Value:'DownRightVectorBar';                Data:'10583'),
    (Value:'DownArrowUpArrow';                  Data:'8693'),
    (Value:'DoubleUpDownArrow';                 Data:'8661'),
    (Value:'DoubleRightTee';                    Data:'8872'),
    (Value:'DoubleRightArrow';                  Data:'8658'),
    (Value:'DoubleUpArrow';                     Data:'8657'),
    (Value:'DownArrow';                         Data:'8595'),
    (Value:'DoubleVerticalBar';                 Data:'8741'),
    (Value:'DownArrowBar';                      Data:'10515'),
    (Value:'DownLeftVector';                    Data:'8637'),
    (Value:'DownLeftRightVector';               Data:'10576'),
    (Value:'DownBreve';                         Data:'785'),
    (Value:'DownLeftTeeVector';                 Data:'10590'),
    (Value:'DownRightTeeVector';                Data:'10591'),
    (Value:'DownLeftVectorBar';                 Data:'10582'),
    (Value:'DownRightVector';                   Data:'8641'),
    (Value:'Eacute';                            Data:'201'),
    (Value:'Dscr';                              Data:'119967'),
    (Value:'DownTeeArrow';                      Data:'8615'),
    (Value:'DownTee';                           Data:'8868'),
    (Value:'Downarrow';                         Data:'8659'),
    (Value:'ENG';                               Data:'330'),
    (Value:'Dstrok';                            Data:'272'),
    (Value:'ETH';                               Data:'208'),
    (Value:'Edot';                              Data:'278'),
    (Value:'Ecirc';                             Data:'202'),
    (Value:'Ecaron';                            Data:'282'),
    (Value:'Ecy';                               Data:'1069'),
    (Value:'Egrave';                            Data:'200'),
    (Value:'Efr';                               Data:'120072'),
    (Value:'Element';                           Data:'8712'),
    (Value:'Emacr';                             Data:'274'),
    (Value:'ImaginaryI';                        Data:'8520'),
    (Value:'Gopf';                              Data:'120126'),
    (Value:'FilledSmallSquare';                 Data:'9724'),
    (Value:'Escr';                              Data:'8496'),
    (Value:'Epsilon';                           Data:'917'),
    (Value:'Eogon';                             Data:'280'),
    (Value:'EmptyVerySmallSquare';              Data:'9643'),
    (Value:'Eopf';                              Data:'120124'),
    (Value:'EqualTilde';                        Data:'8770'),
    (Value:'Equal';                             Data:'10869'),
    (Value:'Equilibrium';                       Data:'8652'),
    (Value:'Exists';                            Data:'8707'),
    (Value:'Eta';                               Data:'919'),
    (Value:'Esim';                              Data:'10867'),
    (Value:'Euml';                              Data:'203'),
    (Value:'Fcy';                               Data:'1060'),
    (Value:'ExponentialE';                      Data:'8519'),
    (Value:'Ffr';                               Data:'120073'),
    (Value:'Gamma';                             Data:'915'),
    (Value:'Fouriertrf';                        Data:'8497'),
    (Value:'Fopf';                              Data:'120125'),
    (Value:'FilledVerySmallSquare';             Data:'9642'),
    (Value:'ForAll';                            Data:'8704'),
    (Value:'GJcy';                              Data:'1027'),
    (Value:'Fscr';                              Data:'8497'),
    (Value:'GT';                                Data:'62'),
    (Value:'Gcirc';                             Data:'284'),
    (Value:'Gbreve';                            Data:'286'),
    (Value:'Gammad';                            Data:'988'),
    (Value:'Gcedil';                            Data:'290'),
    (Value:'Gdot';                              Data:'288'),
    (Value:'Gcy';                               Data:'1043'),
    (Value:'Gfr';                               Data:'120074'),
    (Value:'Gg';                                Data:'8921'),
    (Value:'Hopf';                              Data:'8461'),
    (Value:'Gscr';                              Data:'119970'),
    (Value:'GreaterGreater';                    Data:'10914'),
    (Value:'GreaterEqualLess';                  Data:'8923'),
    (Value:'GreaterEqual';                      Data:'8805'),
    (Value:'GreaterFullEqual';                  Data:'8807'),
    (Value:'GreaterSlantEqual';                 Data:'10878'),
    (Value:'GreaterLess';                       Data:'8823'),
    (Value:'GreaterTilde';                      Data:'8819'),
    (Value:'Hat';                               Data:'94'),
    (Value:'HARDcy';                            Data:'1066'),
    (Value:'Gt';                                Data:'8811'),
    (Value:'Hacek';                             Data:'711'),
    (Value:'Hfr';                               Data:'8460'),
    (Value:'Hcirc';                             Data:'292'),
    (Value:'HilbertSpace';                      Data:'8459'),
    (Value:'IOcy';                              Data:'1025'),
    (Value:'HumpDownHump';                      Data:'8782'),
    (Value:'Hscr';                              Data:'8459'),
    (Value:'HorizontalLine';                    Data:'9472'),
    (Value:'Hstrok';                            Data:'294'),
    (Value:'IEcy';                              Data:'1045'),
    (Value:'HumpEqual';                         Data:'8783'),
    (Value:'IJlig';                             Data:'306'),
    (Value:'Idot';                              Data:'304'),
    (Value:'Icirc';                             Data:'206'),
    (Value:'Iacute';                            Data:'205'),
    (Value:'Icy';                               Data:'1048'),
    (Value:'Igrave';                            Data:'204'),
    (Value:'Ifr';                               Data:'8465'),
    (Value:'Im';                                Data:'8465'),
    (Value:'Imacr';                             Data:'298'),
    (Value:'Lang';                              Data:'10218'),
    (Value:'Jfr';                               Data:'120077'),
    (Value:'Iopf';                              Data:'120128'),
    (Value:'Intersection';                      Data:'8898'),
    (Value:'Int';                               Data:'8748'),
    (Value:'Implies';                           Data:'8658'),
    (Value:'Integral';                          Data:'8747'),
    (Value:'InvisibleTimes';                    Data:'8290'),
    (Value:'InvisibleComma';                    Data:'8291'),
    (Value:'Iogon';                             Data:'302'),
    (Value:'Iukcy';                             Data:'1030'),
    (Value:'Iscr';                              Data:'8464'),
    (Value:'Iota';                              Data:'921'),
    (Value:'Itilde';                            Data:'296'),
    (Value:'Jcirc';                             Data:'308'),
    (Value:'Iuml';                              Data:'207'),
    (Value:'Jcy';                               Data:'1049'),
    (Value:'Kcedil';                            Data:'310'),
    (Value:'Jukcy';                             Data:'1028'),
    (Value:'Jscr';                              Data:'119973'),
    (Value:'Jopf';                              Data:'120129'),
    (Value:'Jsercy';                            Data:'1032'),
    (Value:'KJcy';                              Data:'1036'),
    (Value:'KHcy';                              Data:'1061'),
    (Value:'Kappa';                             Data:'922'),
    (Value:'Kscr';                              Data:'119974'),
    (Value:'Kfr';                               Data:'120078'),
    (Value:'Kcy';                               Data:'1050'),
    (Value:'Kopf';                              Data:'120130'),
    (Value:'LT';                                Data:'60'),
    (Value:'LJcy';                              Data:'1033'),
    (Value:'Lacute';                            Data:'313'),
    (Value:'Lambda';                            Data:'923'),
    (Value:'LeftRightArrow';                    Data:'8596'),
    (Value:'LeftArrowBar';                      Data:'8676'),
    (Value:'Lcedil';                            Data:'315'),
    (Value:'Larr';                              Data:'8606'),
    (Value:'Laplacetrf';                        Data:'8466'),
    (Value:'Lcaron';                            Data:'317'),
    (Value:'LeftAngleBracket';                  Data:'12296'),
    (Value:'Lcy';                               Data:'1051'),
    (Value:'LeftArrow';                         Data:'8592'),
    (Value:'LeftDownTeeVector';                 Data:'10593'),
    (Value:'LeftCeiling';                       Data:'8968'),
    (Value:'LeftArrowRightArrow';               Data:'8646'),
    (Value:'LeftDoubleBracket';                 Data:'10214'),
    (Value:'LeftDownVectorBar';                 Data:'10585'),
    (Value:'LeftDownVector';                    Data:'8643'),
    (Value:'LeftFloor';                         Data:'8970'),
    (Value:'LeftUpDownVector';                  Data:'10577'),
    (Value:'LeftTeeVector';                     Data:'10586'),
    (Value:'LeftTee';                           Data:'8867'),
    (Value:'LeftRightVector';                   Data:'10574'),
    (Value:'LeftTeeArrow';                      Data:'8612'),
    (Value:'LeftTriangleBar';                   Data:'10703'),
    (Value:'LeftTriangle';                      Data:'8882'),
    (Value:'LeftTriangleEqual';                 Data:'8884'),
    (Value:'LeftVector';                        Data:'8636'),
    (Value:'LeftUpVector';                      Data:'8639'),
    (Value:'LeftUpTeeVector';                   Data:'10592'),
    (Value:'LeftUpVectorBar';                   Data:'10584'),
    (Value:'Leftarrow';                         Data:'8656'),
    (Value:'LeftVectorBar';                     Data:'10578'),
    (Value:'Leftrightarrow';                    Data:'8660'),
    (Value:'LessEqualGreater';                  Data:'8922'),
    (Value:'Prime';                             Data:'8243'),
    (Value:'NotLessTilde';                      Data:'8820'),
    (Value:'Ncedil';                            Data:'325'),
    (Value:'LowerLeftArrow';                    Data:'8601'),
    (Value:'Lmidot';                            Data:'319'),
    (Value:'LessTilde';                         Data:'8818'),
    (Value:'LessLess';                          Data:'10913'),
    (Value:'LessGreater';                       Data:'8822'),
    (Value:'LessSlantEqual';                    Data:'10877'),
    (Value:'Ll';                                Data:'8920'),
    (Value:'Lfr';                               Data:'120079'),
    (Value:'Lleftarrow';                        Data:'8666'),
    (Value:'Longleftarrow';                     Data:'10232'),
    (Value:'LongLeftRightArrow';                Data:'10231'),
    (Value:'LongLeftArrow';                     Data:'10229'),
    (Value:'LongRightArrow';                    Data:'10230'),
    (Value:'Longrightarrow';                    Data:'10233'),
    (Value:'Longleftrightarrow';                Data:'10234'),
    (Value:'Lopf';                              Data:'120131'),
    (Value:'Mellintrf';                         Data:'8499'),
    (Value:'Lstrok';                            Data:'321'),
    (Value:'Lscr';                              Data:'8466'),
    (Value:'LowerRightArrow';                   Data:'8600'),
    (Value:'Lsh';                               Data:'8624'),
    (Value:'Map';                               Data:'10501'),
    (Value:'Lt';                                Data:'8810'),
    (Value:'Mcy';                               Data:'1052'),
    (Value:'Mscr';                              Data:'8499'),
    (Value:'MinusPlus';                         Data:'8723'),
    (Value:'Mfr';                               Data:'120080'),
    (Value:'Mopf';                              Data:'120132'),
    (Value:'NJcy';                              Data:'1034'),
    (Value:'Mu';                                Data:'924'),
    (Value:'Nacute';                            Data:'323'),
    (Value:'Ncaron';                            Data:'327'),
    (Value:'NotGreater';                        Data:'8815'),
    (Value:'Not';                               Data:'10988'),
    (Value:'NewLine';                           Data:'9226'),
    (Value:'NestedGreaterGreater';              Data:'8811'),
    (Value:'Ncy';                               Data:'1053'),
    (Value:'NestedLessLess';                    Data:'8810'),
    (Value:'NoBreak';                           Data:'8288'),
    (Value:'Nfr';                               Data:'120081'),
    (Value:'Nopf';                              Data:'8469'),
    (Value:'NotElement';                        Data:'8713'),
    (Value:'NotCupCap';                         Data:'8813'),
    (Value:'NotCongruent';                      Data:'8802'),
    (Value:'NotDoubleVerticalBar';              Data:'8742'),
    (Value:'NotEqualTilde';                     Data:'8770,824'),
    (Value:'NotEqual';                          Data:'8800'),
    (Value:'NotExists';                         Data:'8708'),
    (Value:'NotHumpEqual';                      Data:'8783,824'),
    (Value:'NotGreaterLess';                    Data:'8825'),
    (Value:'NotGreaterFullEqual';               Data:'8807,824'),
    (Value:'NotGreaterEqual';                   Data:'8817'),
    (Value:'NotGreaterGreater';                 Data:'8811,824'),
    (Value:'NotGreaterTilde';                   Data:'8821'),
    (Value:'NotGreaterSlantEqual';              Data:'10878,824'),
    (Value:'NotHumpDownHump';                   Data:'8782,824'),
    (Value:'NotLess';                           Data:'8814'),
    (Value:'NotLeftTriangleBar';                Data:'10703,824'),
    (Value:'NotLeftTriangle';                   Data:'8938'),
    (Value:'NotLeftTriangleEqual';              Data:'8940'),
    (Value:'NotLessGreater';                    Data:'8824'),
    (Value:'NotLessEqual';                      Data:'8816'),
    (Value:'NotLessLess';                       Data:'8810,824'),
    (Value:'NotLessSlantEqual';                 Data:'10877,824'),
    (Value:'Ocy';                               Data:'1054'),
    (Value:'NotSucceeds';                       Data:'8833'),
    (Value:'NotRightTriangleBar';               Data:'10704,824'),
    (Value:'NotPrecedesEqual';                  Data:'10927,824'),
    (Value:'NotNestedLessLess';                 Data:'10913,824'),
    (Value:'NotNestedGreaterGreater';           Data:'10914,824'),
    (Value:'NotPrecedes';                       Data:'8832'),
    (Value:'NotReverseElement';                 Data:'8716'),
    (Value:'NotPrecedesSlantEqual';             Data:'8928'),
    (Value:'NotRightTriangle';                  Data:'8939'),
    (Value:'NotSquareSuperset';                 Data:'8848,824'),
    (Value:'NotSquareSubset';                   Data:'8847,824'),
    (Value:'NotRightTriangleEqual';             Data:'8941'),
    (Value:'NotSquareSubsetEqual';              Data:'8930'),
    (Value:'NotSubset';                         Data:'8834,8402'),
    (Value:'NotSquareSupersetEqual';            Data:'8931'),
    (Value:'NotSubsetEqual';                    Data:'8840'),
    (Value:'NotTildeFullEqual';                 Data:'8775'),
    (Value:'NotSuperset';                       Data:'8835,8402'),
    (Value:'NotSucceedsSlantEqual';             Data:'8929'),
    (Value:'NotSucceedsEqual';                  Data:'10928,824'),
    (Value:'NotSucceedsTilde';                  Data:'8831,824'),
    (Value:'NotTilde';                          Data:'8769'),
    (Value:'NotSupersetEqual';                  Data:'8841'),
    (Value:'NotTildeEqual';                     Data:'8772'),
    (Value:'Ntilde';                            Data:'209'),
    (Value:'NotVerticalBar';                    Data:'8740'),
    (Value:'NotTildeTilde';                     Data:'8777'),
    (Value:'Nscr';                              Data:'119977'),
    (Value:'OElig';                             Data:'338'),
    (Value:'Nu';                                Data:'925'),
    (Value:'Oacute';                            Data:'211'),
    (Value:'Ocirc';                             Data:'212'),
    (Value:'OverBar';                           Data:'8254'),
    (Value:'OpenCurlyDoubleQuote';              Data:'8220'),
    (Value:'Omacr';                             Data:'332'),
    (Value:'Ofr';                               Data:'120082'),
    (Value:'Odblac';                            Data:'336'),
    (Value:'Ograve';                            Data:'210'),
    (Value:'Omicron';                           Data:'927'),
    (Value:'Omega';                             Data:'937'),
    (Value:'Oopf';                              Data:'120134'),
    (Value:'Oslash';                            Data:'216'),
    (Value:'Or';                                Data:'10836'),
    (Value:'OpenCurlyQuote';                    Data:'8216'),
    (Value:'Oscr';                              Data:'119978'),
    (Value:'Otimes';                            Data:'10807'),
    (Value:'Otilde';                            Data:'213'),
    (Value:'Ouml';                              Data:'214'),
    (Value:'Pi';                                Data:'928'),
    (Value:'PartialD';                          Data:'8706'),
    (Value:'OverBracket';                       Data:'9140'),
    (Value:'OverBrace';                         Data:'9182'),
    (Value:'OverParenthesis';                   Data:'9180'),
    (Value:'Pfr';                               Data:'120083'),
    (Value:'Pcy';                               Data:'1055'),
    (Value:'Phi';                               Data:'934'),
    (Value:'Pr';                                Data:'10939'),
    (Value:'Poincareplane';                     Data:'8460'),
    (Value:'PlusMinus';                         Data:'177'),
    (Value:'Popf';                              Data:'8473'),
    (Value:'PrecedesEqual';                     Data:'10927'),
    (Value:'Precedes';                          Data:'8826'),
    (Value:'PrecedesSlantEqual';                Data:'8828'),
    (Value:'PrecedesTilde';                     Data:'8830'),
    (Value:'ShortRightArrow';                   Data:'8594'),
    (Value:'RightDownVectorBar';                Data:'10581'),
    (Value:'Rcaron';                            Data:'344'),
    (Value:'Qopf';                              Data:'8474'),
    (Value:'Pscr';                              Data:'119979'),
    (Value:'Proportion';                        Data:'8759'),
    (Value:'Product';                           Data:'8719'),
    (Value:'Proportional';                      Data:'8733'),
    (Value:'QUOT';                              Data:'34'),
    (Value:'Psi';                               Data:'936'),
    (Value:'Qfr';                               Data:'120084'),
    (Value:'Racute';                            Data:'340'),
    (Value:'RBarr';                             Data:'10512'),
    (Value:'Qscr';                              Data:'119980'),
    (Value:'REG';                               Data:'174'),
    (Value:'Rarr';                              Data:'8608'),
    (Value:'Rang';                              Data:'10219'),
    (Value:'Rarrtl';                            Data:'10518'),
    (Value:'Rho';                               Data:'929'),
    (Value:'ReverseElement';                    Data:'8715'),
    (Value:'Rcy';                               Data:'1056'),
    (Value:'Rcedil';                            Data:'342'),
    (Value:'Re';                                Data:'8476'),
    (Value:'ReverseUpEquilibrium';              Data:'10607'),
    (Value:'ReverseEquilibrium';                Data:'8651'),
    (Value:'Rfr';                               Data:'8476'),
    (Value:'RightArrowLeftArrow';               Data:'8644'),
    (Value:'RightArrow';                        Data:'8594'),
    (Value:'RightAngleBracket';                 Data:'12297'),
    (Value:'RightArrowBar';                     Data:'8677'),
    (Value:'RightDoubleBracket';                Data:'10215'),
    (Value:'RightCeiling';                      Data:'8969'),
    (Value:'RightDownTeeVector';                Data:'10589'),
    (Value:'RightDownVector';                   Data:'8642'),
    (Value:'RoundImplies';                      Data:'10608'),
    (Value:'RightUpDownVector';                 Data:'10575'),
    (Value:'RightTeeVector';                    Data:'10587'),
    (Value:'RightTee';                          Data:'8866'),
    (Value:'RightFloor';                        Data:'8971'),
    (Value:'RightTeeArrow';                     Data:'8614'),
    (Value:'RightTriangleBar';                  Data:'10704'),
    (Value:'RightTriangle';                     Data:'8883'),
    (Value:'RightTriangleEqual';                Data:'8885'),
    (Value:'RightVector';                       Data:'8640'),
    (Value:'RightUpVector';                     Data:'8638'),
    (Value:'RightUpTeeVector';                  Data:'10588'),
    (Value:'RightUpVectorBar';                  Data:'10580'),
    (Value:'Rightarrow';                        Data:'8658'),
    (Value:'RightVectorBar';                    Data:'10579'),
    (Value:'Ropf';                              Data:'8477'),
    (Value:'Sacute';                            Data:'346'),
    (Value:'RuleDelayed';                       Data:'10740'),
    (Value:'Rscr';                              Data:'8475'),
    (Value:'Rrightarrow';                       Data:'8667'),
    (Value:'Rsh';                               Data:'8625'),
    (Value:'SHcy';                              Data:'1064'),
    (Value:'SHCHcy';                            Data:'1065'),
    (Value:'SOFTcy';                            Data:'1068'),
    (Value:'Scirc';                             Data:'348'),
    (Value:'Scaron';                            Data:'352'),
    (Value:'Sc';                                Data:'10940'),
    (Value:'Scedil';                            Data:'350'),
    (Value:'Sfr';                               Data:'120086'),
    (Value:'Scy';                               Data:'1057'),
    (Value:'ShortDownArrow';                    Data:'8595'),
    (Value:'ShortLeftArrow';                    Data:'8592'),
    (Value:'Tau';                               Data:'932'),
    (Value:'Subset';                            Data:'8912'),
    (Value:'SquareSubset';                      Data:'8847'),
    (Value:'Sopf';                              Data:'120138'),
    (Value:'Sigma';                             Data:'931'),
    (Value:'ShortUpArrow';                      Data:'8593'),
    (Value:'SmallCircle';                       Data:'8728'),
    (Value:'Square';                            Data:'9633'),
    (Value:'Sqrt';                              Data:'8730'),
    (Value:'SquareIntersection';                Data:'8851'),
    (Value:'SquareUnion';                       Data:'8852'),
    (Value:'SquareSuperset';                    Data:'8848'),
    (Value:'SquareSubsetEqual';                 Data:'8849'),
    (Value:'SquareSupersetEqual';               Data:'8850'),
    (Value:'Star';                              Data:'8902'),
    (Value:'Sscr';                              Data:'119982'),
    (Value:'Sub';                               Data:'8912'),
    (Value:'Sup';                               Data:'8913'),
    (Value:'SucceedsSlantEqual';                Data:'8829'),
    (Value:'Succeeds';                          Data:'8827'),
    (Value:'SubsetEqual';                       Data:'8838'),
    (Value:'SucceedsEqual';                     Data:'10928'),
    (Value:'SuchThat';                          Data:'8715'),
    (Value:'SucceedsTilde';                     Data:'8831'),
    (Value:'Sum';                               Data:'8721'),
    (Value:'THORN';                             Data:'222'),
    (Value:'SupersetEqual';                     Data:'8839'),
    (Value:'Superset';                          Data:'8835'),
    (Value:'Supset';                            Data:'8913'),
    (Value:'TSHcy';                             Data:'1035'),
    (Value:'TRADE';                             Data:'8482'),
    (Value:'TScy';                              Data:'1062'),
    (Value:'Tab';                               Data:'9225'),
    (Value:'Uarr';                              Data:'8607'),
    (Value:'TildeEqual';                        Data:'8771'),
    (Value:'Tfr';                               Data:'120087'),
    (Value:'Tcedil';                            Data:'354'),
    (Value:'Tcaron';                            Data:'356'),
    (Value:'Tcy';                               Data:'1058'),
    (Value:'Theta';                             Data:'920'),
    (Value:'Therefore';                         Data:'8756'),
    (Value:'Tilde';                             Data:'8764'),
    (Value:'TripleDot';                         Data:'8411'),
    (Value:'TildeTilde';                        Data:'8776'),
    (Value:'TildeFullEqual';                    Data:'8773'),
    (Value:'Topf';                              Data:'120139'),
    (Value:'Tstrok';                            Data:'358'),
    (Value:'Tscr';                              Data:'119983'),
    (Value:'Uacute';                            Data:'218'),
    (Value:'Ugrave';                            Data:'217'),
    (Value:'Ucirc';                             Data:'219'),
    (Value:'Ubrcy';                             Data:'1038'),
    (Value:'Uarrocir';                          Data:'10569'),
    (Value:'Ubreve';                            Data:'364'),
    (Value:'Udblac';                            Data:'368'),
    (Value:'Ucy';                               Data:'1059'),
    (Value:'Ufr';                               Data:'120088'),
    (Value:'UnderBracket';                      Data:'9141'),
    (Value:'UnderBar';                          Data:'95'),
    (Value:'Umacr';                             Data:'362'),
    (Value:'UnderBrace';                        Data:'9183'),
    (Value:'Union';                             Data:'8899'),
    (Value:'UnderParenthesis';                  Data:'9181'),
    (Value:'UnionPlus';                         Data:'8846'),
    (Value:'Uogon';                             Data:'370'),
    (Value:'cir';                               Data:'9675'),
    (Value:'bbrk';                              Data:'9141'),
    (Value:'acE';                               Data:'8766,819'),
    (Value:'Vscr';                              Data:'119985'),
    (Value:'Utilde';                            Data:'360'),
    (Value:'Uparrow';                           Data:'8657'),
    (Value:'UpDownArrow';                       Data:'8597'),
    (Value:'UpArrowBar';                        Data:'10514'),
    (Value:'UpArrow';                           Data:'8593'),
    (Value:'UpArrowDownArrow';                  Data:'8645'),
    (Value:'UpTee';                             Data:'8869'),
    (Value:'UpEquilibrium';                     Data:'10606'),
    (Value:'UpTeeArrow';                        Data:'8613'),
    (Value:'Upsi';                              Data:'978'),
    (Value:'UpperLeftArrow';                    Data:'8598'),
    (Value:'Updownarrow';                       Data:'8661'),
    (Value:'UpperRightArrow';                   Data:'8599'),
    (Value:'Uring';                             Data:'366'),
    (Value:'Upsilon';                           Data:'933'),
    (Value:'Uscr';                              Data:'119984'),
    (Value:'Verbar';                            Data:'8214'),
    (Value:'Vcy';                               Data:'1042'),
    (Value:'VDash';                             Data:'8875'),
    (Value:'Uuml';                              Data:'220'),
    (Value:'Vbar';                              Data:'10987'),
    (Value:'Vdashl';                            Data:'10982'),
    (Value:'Vdash';                             Data:'8873'),
    (Value:'Vee';                               Data:'8897'),
    (Value:'VerticalLine';                      Data:'166'),
    (Value:'VerticalBar';                       Data:'8739'),
    (Value:'Vert';                              Data:'8214'),
    (Value:'VerticalLine';                      Data:'124'),
    (Value:'VerticalTilde';                     Data:'8768'),
    (Value:'VerticalSeparator';                 Data:'10072'),
    (Value:'Vfr';                               Data:'120089'),
    (Value:'Vopf';                              Data:'120141'),
    (Value:'Ycy';                               Data:'1067'),
    (Value:'Xi';                                Data:'926'),
    (Value:'Wfr';                               Data:'120090'),
    (Value:'Wcirc';                             Data:'372'),
    (Value:'Vvdash';                            Data:'8874'),
    (Value:'Wedge';                             Data:'8896'),
    (Value:'Wscr';                              Data:'119986'),
    (Value:'Wopf';                              Data:'120142'),
    (Value:'Xfr';                               Data:'120091'),
    (Value:'YIcy';                              Data:'1031'),
    (Value:'Xscr';                              Data:'119987'),
    (Value:'Xopf';                              Data:'120143'),
    (Value:'YAcy';                              Data:'1071'),
    (Value:'Yacute';                            Data:'221'),
    (Value:'YUcy';                              Data:'1070'),
    (Value:'Ycirc';                             Data:'374'),
    (Value:'Zcy';                               Data:'1047'),
    (Value:'Yuml';                              Data:'376'),
    (Value:'Yopf';                              Data:'120144'),
    (Value:'Yfr';                               Data:'120092'),
    (Value:'Yscr';                              Data:'119988'),
    (Value:'Zacute';                            Data:'377'),
    (Value:'ZHcy';                              Data:'1046'),
    (Value:'Zcaron';                            Data:'381'),
    (Value:'Zopf';                              Data:'8484'),
    (Value:'Zeta';                              Data:'918'),
    (Value:'Zdot';                              Data:'379'),
    (Value:'Zfr';                               Data:'8488'),
    (Value:'aacute';                            Data:'225'),
    (Value:'Zscr';                              Data:'119989'),
    (Value:'abreve';                            Data:'259'),
    (Value:'ac';                                Data:'8766'),
    (Value:'angrtvb';                           Data:'8894'),
    (Value:'andand';                            Data:'10837'),
    (Value:'agrave';                            Data:'224'),
    (Value:'acy';                               Data:'1072'),
    (Value:'acirc';                             Data:'226'),
    (Value:'acd';                               Data:'8767'),
    (Value:'acute';                             Data:'180'),
    (Value:'af';                                Data:'8289'),
    (Value:'aelig';                             Data:'230'),
    (Value:'afr';                               Data:'120094'),
    (Value:'amacr';                             Data:'257'),
    (Value:'aleph';                             Data:'8501'),
    (Value:'alefsym';                           Data:'8501'),
    (Value:'alpha';                             Data:'945'),
    (Value:'amp';                               Data:'38'),
    (Value:'amalg';                             Data:'10815'),
    (Value:'and';                               Data:'8743'),
    (Value:'angmsdaa';                          Data:'10664'),
    (Value:'ang';                               Data:'8736'),
    (Value:'andslope';                          Data:'10840'),
    (Value:'andd';                              Data:'10844'),
    (Value:'andv';                              Data:'10842'),
    (Value:'angle';                             Data:'8736'),
    (Value:'ange';                              Data:'10660'),
    (Value:'angmsd';                            Data:'8737'),
    (Value:'angmsdae';                          Data:'10668'),
    (Value:'angmsdac';                          Data:'10666'),
    (Value:'angmsdab';                          Data:'10665'),
    (Value:'angmsdad';                          Data:'10667'),
    (Value:'angmsdag';                          Data:'10670'),
    (Value:'angmsdaf';                          Data:'10669'),
    (Value:'angmsdah';                          Data:'10671'),
    (Value:'angrt';                             Data:'8735'),
    (Value:'ascr';                              Data:'119990'),
    (Value:'apE';                               Data:'10864'),
    (Value:'angzarr';                           Data:'9084'),
    (Value:'angsph';                            Data:'8738'),
    (Value:'angrtvbd';                          Data:'10653'),
    (Value:'angst';                             Data:'197'),
    (Value:'aopf';                              Data:'120146'),
    (Value:'aogon';                             Data:'261'),
    (Value:'ap';                                Data:'8776'),
    (Value:'apos';                              Data:'39'),
    (Value:'ape';                               Data:'8778'),
    (Value:'apacir';                            Data:'10863'),
    (Value:'apid';                              Data:'8779'),
    (Value:'approxeq';                          Data:'8778'),
    (Value:'approx';                            Data:'8776'),
    (Value:'aring';                             Data:'229'),
    (Value:'bNot';                              Data:'10989'),
    (Value:'atilde';                            Data:'227'),
    (Value:'asymp';                             Data:'8776'),
    (Value:'ast';                               Data:'42'),
    (Value:'asympeq';                           Data:'8781'),
    (Value:'awconint';                          Data:'8755'),
    (Value:'auml';                              Data:'228'),
    (Value:'awint';                             Data:'10769'),
    (Value:'backsim';                           Data:'8765'),
    (Value:'backepsilon';                       Data:'1014'),
    (Value:'backcong';                          Data:'8780'),
    (Value:'backprime';                         Data:'8245'),
    (Value:'barvee';                            Data:'8893'),
    (Value:'backsimeq';                         Data:'8909'),
    (Value:'barwed';                            Data:'8965'),
    (Value:'barwedge';                          Data:'8965'),
    (Value:'boxbox';                            Data:'10697'),
    (Value:'blacktriangleright';                Data:'9656'),
    (Value:'bigcup';                            Data:'8899'),
    (Value:'bepsi';                             Data:'1014'),
    (Value:'bdquo';                             Data:'8222'),
    (Value:'bcong';                             Data:'8780'),
    (Value:'bbrktbrk';                          Data:'9142'),
    (Value:'bcy';                               Data:'1073'),
    (Value:'because';                           Data:'8757'),
    (Value:'becaus';                            Data:'8757'),
    (Value:'bemptyv';                           Data:'10672'),
    (Value:'between';                           Data:'8812'),
    (Value:'beta';                              Data:'946'),
    (Value:'bernou';                            Data:'8492'),
    (Value:'beth';                              Data:'8502'),
    (Value:'bigcap';                            Data:'8898'),
    (Value:'bfr';                               Data:'120095'),
    (Value:'bigcirc';                           Data:'9711'),
    (Value:'biguplus';                          Data:'10756'),
    (Value:'bigsqcup';                          Data:'10758'),
    (Value:'bigoplus';                          Data:'10753'),
    (Value:'bigodot';                           Data:'10752'),
    (Value:'bigotimes';                         Data:'10754'),
    (Value:'bigtriangledown';                   Data:'9661'),
    (Value:'bigstar';                           Data:'9733'),
    (Value:'bigtriangleup';                     Data:'9651'),
    (Value:'blacklozenge';                      Data:'10731'),
    (Value:'bigwedge';                          Data:'8896'),
    (Value:'bigvee';                            Data:'8897'),
    (Value:'bkarow';                            Data:'10509'),
    (Value:'blacktriangle';                     Data:'9652'),
    (Value:'blacksquare';                       Data:'9642'),
    (Value:'blacktriangledown';                 Data:'9662'),
    (Value:'blacktriangleleft';                 Data:'9666'),
    (Value:'boxDr';                             Data:'9555'),
    (Value:'bnot';                              Data:'8976'),
    (Value:'blk34';                             Data:'9619'),
    (Value:'blk12';                             Data:'9618'),
    (Value:'blank';                             Data:'9251'),
    (Value:'blk14';                             Data:'9617'),
    (Value:'bne';                               Data:'61,8421'),
    (Value:'block';                             Data:'9608'),
    (Value:'bnequiv';                           Data:'8801,8421'),
    (Value:'bowtie';                            Data:'8904'),
    (Value:'bot';                               Data:'8869'),
    (Value:'bopf';                              Data:'120147'),
    (Value:'bottom';                            Data:'8869'),
    (Value:'boxDR';                             Data:'9556'),
    (Value:'boxDL';                             Data:'9559'),
    (Value:'boxDl';                             Data:'9558'),
    (Value:'boxUl';                             Data:'9564'),
    (Value:'boxHd';                             Data:'9572'),
    (Value:'boxHD';                             Data:'9574'),
    (Value:'boxH';                              Data:'9552'),
    (Value:'boxHU';                             Data:'9577'),
    (Value:'boxUL';                             Data:'9565'),
    (Value:'boxHu';                             Data:'9575'),
    (Value:'boxUR';                             Data:'9562'),
    (Value:'boxVL';                             Data:'9571'),
    (Value:'boxV';                              Data:'9553'),
    (Value:'boxUr';                             Data:'9561'),
    (Value:'boxVH';                             Data:'9580'),
    (Value:'boxVh';                             Data:'9579'),
    (Value:'boxVR';                             Data:'9568'),
    (Value:'boxVl';                             Data:'9570'),
    (Value:'boxVr';                             Data:'9567'),
    (Value:'bsolhsub';                          Data:'10184'),
    (Value:'boxur';                             Data:'9492'),
    (Value:'boxhd';                             Data:'9516'),
    (Value:'boxdr';                             Data:'9484'),
    (Value:'boxdR';                             Data:'9554'),
    (Value:'boxdL';                             Data:'9557'),
    (Value:'boxdl';                             Data:'9488'),
    (Value:'boxhD';                             Data:'9573'),
    (Value:'boxh';                              Data:'9472'),
    (Value:'boxhU';                             Data:'9576'),
    (Value:'boxtimes';                          Data:'8864'),
    (Value:'boxminus';                          Data:'8863'),
    (Value:'boxhu';                             Data:'9524'),
    (Value:'boxplus';                           Data:'8862'),
    (Value:'boxuR';                             Data:'9560'),
    (Value:'boxuL';                             Data:'9563'),
    (Value:'boxul';                             Data:'9496'),
    (Value:'bprime';                            Data:'8245'),
    (Value:'boxvR';                             Data:'9566'),
    (Value:'boxvH';                             Data:'9578'),
    (Value:'boxv';                              Data:'9474'),
    (Value:'boxvL';                             Data:'9569'),
    (Value:'boxvl';                             Data:'9508'),
    (Value:'boxvh';                             Data:'9532'),
    (Value:'boxvr';                             Data:'9500'),
    (Value:'bsemi';                             Data:'8271'),
    (Value:'brvbar';                            Data:'166'),
    (Value:'breve';                             Data:'728'),
    (Value:'bscr';                              Data:'119991'),
    (Value:'bsime';                             Data:'8909'),
    (Value:'bsim';                              Data:'8765'),
    (Value:'bsol';                              Data:'92'),
    (Value:'bsolb';                             Data:'10693'),
    (Value:'caron';                             Data:'711'),
    (Value:'cap';                               Data:'8745'),
    (Value:'bumpE';                             Data:'10926'),
    (Value:'bullet';                            Data:'8226'),
    (Value:'bull';                              Data:'8226'),
    (Value:'bump';                              Data:'8782'),
    (Value:'bumpeq';                            Data:'8783'),
    (Value:'bumpe';                             Data:'8783'),
    (Value:'cacute';                            Data:'263'),
    (Value:'capcup';                            Data:'10823'),
    (Value:'capbrcup';                          Data:'10825'),
    (Value:'capand';                            Data:'10820'),
    (Value:'capcap';                            Data:'10827'),
    (Value:'caps';                              Data:'8745,65024'),
    (Value:'capdot';                            Data:'10816'),
    (Value:'caret';                             Data:'8257'),
    (Value:'cedil';                             Data:'184'),
    (Value:'ccirc';                             Data:'265'),
    (Value:'ccaron';                            Data:'269'),
    (Value:'ccaps';                             Data:'10829'),
    (Value:'ccedil';                            Data:'231'),
    (Value:'ccupssm';                           Data:'10832'),
    (Value:'ccups';                             Data:'10828'),
    (Value:'cdot';                              Data:'267'),
    (Value:'cfr';                               Data:'120096'),
    (Value:'cent';                              Data:'162'),
    (Value:'cemptyv';                           Data:'10674'),
    (Value:'centerdot';                         Data:'183'),
    (Value:'check';                             Data:'10003'),
    (Value:'chcy';                              Data:'1095'),
    (Value:'checkmark';                         Data:'10003'),
    (Value:'chi';                               Data:'967'),
    (Value:'eDDot';                             Data:'10871'),
    (Value:'cwint';                             Data:'8753'),
    (Value:'crarr';                             Data:'8629'),
    (Value:'clubsuit';                          Data:'9827'),
    (Value:'circledast';                        Data:'8859'),
    (Value:'circlearrowleft';                   Data:'8634'),
    (Value:'circ';                              Data:'710'),
    (Value:'cirE';                              Data:'10691'),
    (Value:'circeq';                            Data:'8791'),
    (Value:'circledR';                          Data:'174'),
    (Value:'circlearrowright';                  Data:'8635'),
    (Value:'circledS';                          Data:'9416'),
    (Value:'cirfnint';                          Data:'10768'),
    (Value:'circleddash';                       Data:'8861'),
    (Value:'circledcirc';                       Data:'8858'),
    (Value:'cire';                              Data:'8791'),
    (Value:'cirscir';                           Data:'10690'),
    (Value:'cirmid';                            Data:'10991'),
    (Value:'clubs';                             Data:'9827'),
    (Value:'complement';                        Data:'8705'),
    (Value:'comma';                             Data:'44'),
    (Value:'colone';                            Data:'8788'),
    (Value:'colon';                             Data:'58'),
    (Value:'coloneq';                           Data:'8788'),
    (Value:'comp';                              Data:'8705'),
    (Value:'commat';                            Data:'64'),
    (Value:'compfn';                            Data:'8728'),
    (Value:'conint';                            Data:'8750'),
    (Value:'cong';                              Data:'8773'),
    (Value:'complexes';                         Data:'8450'),
    (Value:'congdot';                           Data:'10861'),
    (Value:'coprod';                            Data:'8720'),
    (Value:'copf';                              Data:'120148'),
    (Value:'copy';                              Data:'169'),
    (Value:'copysr';                            Data:'8471'),
    (Value:'cupcap';                            Data:'10822'),
    (Value:'cudarrl';                           Data:'10552'),
    (Value:'csube';                             Data:'10961'),
    (Value:'cscr';                              Data:'119992'),
    (Value:'cross';                             Data:'10007'),
    (Value:'csub';                              Data:'10959'),
    (Value:'csupe';                             Data:'10962'),
    (Value:'csup';                              Data:'10960'),
    (Value:'ctdot';                             Data:'8943'),
    (Value:'cularr';                            Data:'8630'),
    (Value:'cuepr';                             Data:'8926'),
    (Value:'cudarrr';                           Data:'10549'),
    (Value:'cuesc';                             Data:'8927'),
    (Value:'cup';                               Data:'8746'),
    (Value:'cularrp';                           Data:'10557'),
    (Value:'cupbrcap';                          Data:'10824'),
    (Value:'curlyeqsucc';                       Data:'8927'),
    (Value:'cups';                              Data:'8746,65024'),
    (Value:'cupdot';                            Data:'8845'),
    (Value:'cupcup';                            Data:'10826'),
    (Value:'cupor';                             Data:'10821'),
    (Value:'curarrm';                           Data:'10556'),
    (Value:'curarr';                            Data:'8631'),
    (Value:'curlyeqprec';                       Data:'8926'),
    (Value:'curvearrowleft';                    Data:'8630'),
    (Value:'curlywedge';                        Data:'8911'),
    (Value:'curlyvee';                          Data:'8910'),
    (Value:'curren';                            Data:'164'),
    (Value:'cuvee';                             Data:'8910'),
    (Value:'curvearrowright';                   Data:'8631'),
    (Value:'cuwed';                             Data:'8911'),
    (Value:'cwconint';                          Data:'8754'),
    (Value:'divideontimes';                     Data:'8903'),
    (Value:'ddotseq';                           Data:'10871'),
    (Value:'dashv';                             Data:'8867'),
    (Value:'dagger';                            Data:'8224'),
    (Value:'dArr';                              Data:'8659'),
    (Value:'cylcty';                            Data:'9005'),
    (Value:'dHar';                              Data:'10597'),
    (Value:'darr';                              Data:'8595'),
    (Value:'daleth';                            Data:'8504'),
    (Value:'dash';                              Data:'8208'),
    (Value:'dcy';                               Data:'1076'),
    (Value:'dblac';                             Data:'733'),
    (Value:'dbkarow';                           Data:'10511'),
    (Value:'dcaron';                            Data:'271'),
    (Value:'ddagger';                           Data:'8225'),
    (Value:'dd';                                Data:'8518'),
    (Value:'ddarr';                             Data:'8650'),
    (Value:'diam';                              Data:'8900'),
    (Value:'dfisht';                            Data:'10623'),
    (Value:'delta';                             Data:'948'),
    (Value:'deg';                               Data:'176'),
    (Value:'demptyv';                           Data:'10673'),
    (Value:'dharl';                             Data:'8643'),
    (Value:'dfr';                               Data:'120097'),
    (Value:'dharr';                             Data:'8642'),
    (Value:'die';                               Data:'168'),
    (Value:'diamondsuit';                       Data:'9830'),
    (Value:'diamond';                           Data:'8900'),
    (Value:'diams';                             Data:'9830'),
    (Value:'disin';                             Data:'8946'),
    (Value:'digamma';                           Data:'989'),
    (Value:'div';                               Data:'247'),
    (Value:'divide';                            Data:'247'),
    (Value:'downharpoonleft';                   Data:'8643'),
    (Value:'doteq';                             Data:'8784'),
    (Value:'dlcrop';                            Data:'8973'),
    (Value:'djcy';                              Data:'1106'),
    (Value:'divonx';                            Data:'8903'),
    (Value:'dlcorn';                            Data:'8990'),
    (Value:'dopf';                              Data:'120149'),
    (Value:'dollar';                            Data:'36'),
    (Value:'dot';                               Data:'729'),
    (Value:'dotsquare';                         Data:'8865'),
    (Value:'dotminus';                          Data:'8760'),
    (Value:'doteqdot';                          Data:'8785'),
    (Value:'dotplus';                           Data:'8724'),
    (Value:'downarrow';                         Data:'8595'),
    (Value:'doublebarwedge';                    Data:'8966'),
    (Value:'downdownarrows';                    Data:'8650'),
    (Value:'dstrok';                            Data:'273'),
    (Value:'drcrop';                            Data:'8972'),
    (Value:'drbkarow';                          Data:'10512'),
    (Value:'downharpoonright';                  Data:'8642'),
    (Value:'drcorn';                            Data:'8991'),
    (Value:'dscy';                              Data:'1109'),
    (Value:'dscr';                              Data:'119993'),
    (Value:'dsol';                              Data:'10742'),
    (Value:'duarr';                             Data:'8693'),
    (Value:'dtri';                              Data:'9663'),
    (Value:'dtdot';                             Data:'8945'),
    (Value:'dtrif';                             Data:'9662'),
    (Value:'dwangle';                           Data:'10662'),
    (Value:'duhar';                             Data:'10607'),
    (Value:'dzcy';                              Data:'1119'),
    (Value:'dzigrarr';                          Data:'10239'),
    (Value:'fjlig';                             Data:'102,106'),
    (Value:'epsilon';                           Data:'949'),
    (Value:'egsdot';                            Data:'10904'),
    (Value:'ecy';                               Data:'1101'),
    (Value:'ecaron';                            Data:'283'),
    (Value:'eacute';                            Data:'233'),
    (Value:'eDot';                              Data:'8785'),
    (Value:'easter';                            Data:'10862'),
    (Value:'ecirc';                             Data:'234'),
    (Value:'ecir';                              Data:'8790'),
    (Value:'ecolon';                            Data:'8789'),
    (Value:'efr';                               Data:'120098'),
    (Value:'ee';                                Data:'8519'),
    (Value:'edot';                              Data:'279'),
    (Value:'efDot';                             Data:'8786'),
    (Value:'egrave';                            Data:'232'),
    (Value:'eg';                                Data:'10906'),
    (Value:'egs';                               Data:'10902'),
    (Value:'emptyset';                          Data:'8709'),
    (Value:'els';                               Data:'10901'),
    (Value:'elinters';                          Data:'9191'),
    (Value:'el';                                Data:'10905'),
    (Value:'ell';                               Data:'8467'),
    (Value:'emacr';                             Data:'275'),
    (Value:'elsdot';                            Data:'10903'),
    (Value:'empty';                             Data:'8709'),
    (Value:'eopf';                              Data:'120150'),
    (Value:'eng';                               Data:'331'),
    (Value:'emptyv';                            Data:'8709'),
    (Value:'eogon';                             Data:'281'),
    (Value:'eparsl';                            Data:'10723'),
    (Value:'epar';                              Data:'8917'),
    (Value:'eplus';                             Data:'10865'),
    (Value:'epsi';                              Data:'949'),
    (Value:'esim';                              Data:'8770'),
    (Value:'equest';                            Data:'8799'),
    (Value:'eqsim';                             Data:'8770'),
    (Value:'eqcirc';                            Data:'8790'),
    (Value:'epsiv';                             Data:'1013'),
    (Value:'eqcolon';                           Data:'8789'),
    (Value:'eqslantless';                       Data:'10901'),
    (Value:'eqslantgtr';                        Data:'10902'),
    (Value:'equals';                            Data:'61'),
    (Value:'erDot';                             Data:'8787'),
    (Value:'equivDD';                           Data:'10872'),
    (Value:'equiv';                             Data:'8801'),
    (Value:'eqvparsl';                          Data:'10725'),
    (Value:'escr';                              Data:'8495'),
    (Value:'erarr';                             Data:'10609'),
    (Value:'esdot';                             Data:'8784'),
    (Value:'exponentiale';                      Data:'8519'),
    (Value:'euro';                              Data:'8364'),
    (Value:'eth';                               Data:'240'),
    (Value:'eta';                               Data:'951'),
    (Value:'euml';                              Data:'235'),
    (Value:'exist';                             Data:'8707'),
    (Value:'excl';                              Data:'33'),
    (Value:'expectation';                       Data:'8496'),
    (Value:'ffilig';                            Data:'64259'),
    (Value:'fcy';                               Data:'1092'),
    (Value:'fallingdotseq';                     Data:'8786'),
    (Value:'female';                            Data:'9792'),
    (Value:'ffllig';                            Data:'64260'),
    (Value:'fflig';                             Data:'64256'),
    (Value:'ffr';                               Data:'120099'),
    (Value:'filig';                             Data:'64257'),
    (Value:'gap';                               Data:'10886'),
    (Value:'frac23';                            Data:'8532'),
    (Value:'forkv';                             Data:'10969'),
    (Value:'fnof';                              Data:'402'),
    (Value:'fllig';                             Data:'64258'),
    (Value:'flat';                              Data:'9837'),
    (Value:'fltns';                             Data:'9649'),
    (Value:'forall';                            Data:'8704'),
    (Value:'fopf';                              Data:'120151'),
    (Value:'fork';                              Data:'8916'),
    (Value:'frac14';                            Data:'188'),
    (Value:'frac12';                            Data:'189'),
    (Value:'fpartint';                          Data:'10765'),
    (Value:'frac13';                            Data:'8531'),
    (Value:'frac16';                            Data:'8537'),
    (Value:'frac15';                            Data:'8533'),
    (Value:'frac18';                            Data:'8539'),
    (Value:'frac78';                            Data:'8542'),
    (Value:'frac38';                            Data:'8540'),
    (Value:'frac34';                            Data:'190'),
    (Value:'frac25';                            Data:'8534'),
    (Value:'frac35';                            Data:'8535'),
    (Value:'frac56';                            Data:'8538'),
    (Value:'frac45';                            Data:'8536'),
    (Value:'frac58';                            Data:'8541'),
    (Value:'gE';                                Data:'8807'),
    (Value:'frown';                             Data:'8994'),
    (Value:'frasl';                             Data:'8260'),
    (Value:'fscr';                              Data:'119995'),
    (Value:'gacute';                            Data:'501'),
    (Value:'gEl';                               Data:'10892'),
    (Value:'gamma';                             Data:'947'),
    (Value:'gammad';                            Data:'989'),
    (Value:'gesles';                            Data:'10900'),
    (Value:'geqq';                              Data:'8807'),
    (Value:'gdot';                              Data:'289'),
    (Value:'gcirc';                             Data:'285'),
    (Value:'gbreve';                            Data:'287'),
    (Value:'gcy';                               Data:'1075'),
    (Value:'gel';                               Data:'8923'),
    (Value:'ge';                                Data:'8805'),
    (Value:'geq';                               Data:'8805'),
    (Value:'gesdot';                            Data:'10880'),
    (Value:'ges';                               Data:'10878'),
    (Value:'geqslant';                          Data:'10878'),
    (Value:'gescc';                             Data:'10921'),
    (Value:'gesdotol';                          Data:'10884'),
    (Value:'gesdoto';                           Data:'10882'),
    (Value:'gesl';                              Data:'8923,65024'),
    (Value:'gla';                               Data:'10917'),
    (Value:'gimel';                             Data:'8503'),
    (Value:'gg';                                Data:'8811'),
    (Value:'gfr';                               Data:'120100'),
    (Value:'ggg';                               Data:'8921'),
    (Value:'gl';                                Data:'8823'),
    (Value:'gjcy';                              Data:'1107'),
    (Value:'glE';                               Data:'10898'),
    (Value:'gnapprox';                          Data:'10890'),
    (Value:'gnE';                               Data:'8809'),
    (Value:'glj';                               Data:'10916'),
    (Value:'gnap';                              Data:'10890'),
    (Value:'gneq';                              Data:'10888'),
    (Value:'gne';                               Data:'10888'),
    (Value:'gneqq';                             Data:'8809'),
    (Value:'gnsim';                             Data:'8935'),
    (Value:'part';                              Data:'8706'),
    (Value:'lurdshar';                          Data:'10570'),
    (Value:'laquo';                             Data:'171'),
    (Value:'imagline';                          Data:'8464'),
    (Value:'hellip';                            Data:'8230'),
    (Value:'gtreqqless';                        Data:'10892'),
    (Value:'gtcir';                             Data:'10874'),
    (Value:'gsime';                             Data:'10894'),
    (Value:'gscr';                              Data:'8458'),
    (Value:'grave';                             Data:'96'),
    (Value:'gsim';                              Data:'8819'),
    (Value:'gt';                                Data:'62'),
    (Value:'gsiml';                             Data:'10896'),
    (Value:'gtcc';                              Data:'10919'),
    (Value:'gtrapprox';                         Data:'10886'),
    (Value:'gtlPar';                            Data:'10645'),
    (Value:'gtdot';                             Data:'8919'),
    (Value:'gtquest';                           Data:'10876'),
    (Value:'gtrdot';                            Data:'8919'),
    (Value:'gtrarr';                            Data:'10616'),
    (Value:'gtreqless';                         Data:'8923'),
    (Value:'hardcy';                            Data:'1098'),
    (Value:'gvnE';                              Data:'8809,65024'),
    (Value:'gtrsim';                            Data:'8819'),
    (Value:'gtrless';                           Data:'8823'),
    (Value:'gvertneqq';                         Data:'8809,65024'),
    (Value:'half';                              Data:'189'),
    (Value:'hArr';                              Data:'8660'),
    (Value:'hamilt';                            Data:'8459'),
    (Value:'hbar';                              Data:'8463'),
    (Value:'harrcir';                           Data:'10568'),
    (Value:'harr';                              Data:'8596'),
    (Value:'harrw';                             Data:'8621'),
    (Value:'hearts';                            Data:'9829'),
    (Value:'hcirc';                             Data:'293'),
    (Value:'heartsuit';                         Data:'9829'),
    (Value:'iacute';                            Data:'237'),
    (Value:'hookrightarrow';                    Data:'8618'),
    (Value:'hkswarow';                          Data:'10534'),
    (Value:'hfr';                               Data:'120101'),
    (Value:'hercon';                            Data:'8889'),
    (Value:'hksearow';                          Data:'10533'),
    (Value:'homtht';                            Data:'8763'),
    (Value:'hoarr';                             Data:'8703'),
    (Value:'hookleftarrow';                     Data:'8617'),
    (Value:'hslash';                            Data:'8463'),
    (Value:'horbar';                            Data:'8213'),
    (Value:'hopf';                              Data:'120153'),
    (Value:'hscr';                              Data:'119997'),
    (Value:'hybull';                            Data:'8259'),
    (Value:'hstrok';                            Data:'295'),
    (Value:'hyphen';                            Data:'8208'),
    (Value:'igrave';                            Data:'236'),
    (Value:'iecy';                              Data:'1077'),
    (Value:'icirc';                             Data:'238'),
    (Value:'ic';                                Data:'8291'),
    (Value:'icy';                               Data:'1080'),
    (Value:'iff';                               Data:'8660'),
    (Value:'iexcl';                             Data:'161'),
    (Value:'ifr';                               Data:'120102'),
    (Value:'iinfin';                            Data:'10716'),
    (Value:'iiiint';                            Data:'10764'),
    (Value:'ii';                                Data:'8520'),
    (Value:'iiint';                             Data:'8749'),
    (Value:'ijlig';                             Data:'307'),
    (Value:'iiota';                             Data:'8489'),
    (Value:'imacr';                             Data:'299'),
    (Value:'image';                             Data:'8465'),
    (Value:'jcirc';                             Data:'309'),
    (Value:'iocy';                              Data:'1105'),
    (Value:'infintie';                          Data:'10717'),
    (Value:'imped';                             Data:'437'),
    (Value:'imath';                             Data:'305'),
    (Value:'imagpart';                          Data:'8465'),
    (Value:'imof';                              Data:'8887'),
    (Value:'incare';                            Data:'8453'),
    (Value:'in';                                Data:'8712'),
    (Value:'infin';                             Data:'8734'),
    (Value:'integers';                          Data:'8484'),
    (Value:'int';                               Data:'8747'),
    (Value:'inodot';                            Data:'305'),
    (Value:'intcal';                            Data:'8890'),
    (Value:'intlarhk';                          Data:'10775'),
    (Value:'intercal';                          Data:'8890'),
    (Value:'intprod';                           Data:'10812'),
    (Value:'isinE';                             Data:'8953'),
    (Value:'iprod';                             Data:'10812'),
    (Value:'iopf';                              Data:'120154'),
    (Value:'iogon';                             Data:'303'),
    (Value:'iota';                              Data:'953'),
    (Value:'iscr';                              Data:'119998'),
    (Value:'iquest';                            Data:'191'),
    (Value:'isin';                              Data:'8712'),
    (Value:'isinv';                             Data:'8712'),
    (Value:'isins';                             Data:'8948'),
    (Value:'isindot';                           Data:'8949'),
    (Value:'isinsv';                            Data:'8947'),
    (Value:'itilde';                            Data:'297'),
    (Value:'it';                                Data:'8290'),
    (Value:'iukcy';                             Data:'1110'),
    (Value:'iuml';                              Data:'239'),
    (Value:'kopf';                              Data:'120156'),
    (Value:'kappa';                             Data:'954'),
    (Value:'jopf';                              Data:'120155'),
    (Value:'jfr';                               Data:'120103'),
    (Value:'jcy';                               Data:'1081'),
    (Value:'jmath';                             Data:'567'),
    (Value:'jsercy';                            Data:'1112'),
    (Value:'jscr';                              Data:'119999'),
    (Value:'jukcy';                             Data:'1108'),
    (Value:'kfr';                               Data:'120104'),
    (Value:'kcedil';                            Data:'311'),
    (Value:'kappav';                            Data:'1008'),
    (Value:'kcy';                               Data:'1082'),
    (Value:'khcy';                              Data:'1093'),
    (Value:'kgreen';                            Data:'312'),
    (Value:'kjcy';                              Data:'1116'),
    (Value:'lHar';                              Data:'10594'),
    (Value:'lAtail';                            Data:'10523'),
    (Value:'lAarr';                             Data:'8666'),
    (Value:'kscr';                              Data:'120000'),
    (Value:'lArr';                              Data:'8656'),
    (Value:'lE';                                Data:'8806'),
    (Value:'lBarr';                             Data:'10510'),
    (Value:'lEg';                               Data:'10891'),
    (Value:'lambda';                            Data:'955'),
    (Value:'laemptyv';                          Data:'10676'),
    (Value:'lacute';                            Data:'314'),
    (Value:'lagran';                            Data:'8466'),
    (Value:'langd';                             Data:'10641'),
    (Value:'lang';                              Data:'12296'),
    (Value:'langle';                            Data:'12296'),
    (Value:'lap';                               Data:'10885'),
    (Value:'lharu';                             Data:'8636'),
    (Value:'leftarrow';                         Data:'8592'),
    (Value:'lbrace';                            Data:'123'),
    (Value:'larrsim';                           Data:'10611'),
    (Value:'larrfs';                            Data:'10525'),
    (Value:'larrb';                             Data:'8676'),
    (Value:'larr';                              Data:'8592'),
    (Value:'larrbfs';                           Data:'10527'),
    (Value:'larrlp';                            Data:'8619'),
    (Value:'larrhk';                            Data:'8617'),
    (Value:'larrpl';                            Data:'10553'),
    (Value:'late';                              Data:'10925'),
    (Value:'lat';                               Data:'10923'),
    (Value:'larrtl';                            Data:'8610'),
    (Value:'latail';                            Data:'10521'),
    (Value:'lbarr';                             Data:'10508'),
    (Value:'lates';                             Data:'10925,65024'),
    (Value:'lbbrk';                             Data:'10098'),
    (Value:'lcub';                              Data:'123'),
    (Value:'lbrkslu';                           Data:'10637'),
    (Value:'lbrke';                             Data:'10635'),
    (Value:'lbrack';                            Data:'91'),
    (Value:'lbrksld';                           Data:'10639'),
    (Value:'lcedil';                            Data:'316'),
    (Value:'lcaron';                            Data:'318'),
    (Value:'lceil';                             Data:'8968'),
    (Value:'ldquor';                            Data:'8222'),
    (Value:'ldca';                              Data:'10550'),
    (Value:'lcy';                               Data:'1083'),
    (Value:'ldquo';                             Data:'8220'),
    (Value:'ldrushar';                          Data:'10571'),
    (Value:'ldrdhar';                           Data:'10599'),
    (Value:'ldsh';                              Data:'8626'),
    (Value:'le';                                Data:'8804'),
    (Value:'lesdot';                            Data:'10879'),
    (Value:'leftrightsquigarrow';               Data:'8621'),
    (Value:'leftleftarrows';                    Data:'8647'),
    (Value:'leftharpoondown';                   Data:'8637'),
    (Value:'leftarrowtail';                     Data:'8610'),
    (Value:'leftharpoonup';                     Data:'8636'),
    (Value:'leftrightarrows';                   Data:'8646'),
    (Value:'leftrightarrow';                    Data:'8596'),
    (Value:'leftrightharpoons';                 Data:'8651'),
    (Value:'leqq';                              Data:'8806'),
    (Value:'leg';                               Data:'8922'),
    (Value:'leftthreetimes';                    Data:'8907'),
    (Value:'leq';                               Data:'8804'),
    (Value:'les';                               Data:'10877'),
    (Value:'leqslant';                          Data:'10877'),
    (Value:'lescc';                             Data:'10920'),
    (Value:'lesseqqgtr';                        Data:'10891'),
    (Value:'lesges';                            Data:'10899'),
    (Value:'lesdotor';                          Data:'10883'),
    (Value:'lesdoto';                           Data:'10881'),
    (Value:'lesg';                              Data:'8922,65024'),
    (Value:'lessdot';                           Data:'8918'),
    (Value:'lessapprox';                        Data:'10885'),
    (Value:'lesseqgtr';                         Data:'8922'),
    (Value:'lfloor';                            Data:'8970'),
    (Value:'lesssim';                           Data:'8818'),
    (Value:'lessgtr';                           Data:'8822'),
    (Value:'lfisht';                            Data:'10620'),
    (Value:'lg';                                Data:'8822'),
    (Value:'lfr';                               Data:'120105'),
    (Value:'lgE';                               Data:'10897'),
    (Value:'lhard';                             Data:'8637'),
    (Value:'lowbar';                            Data:'95'),
    (Value:'lneq';                              Data:'10887'),
    (Value:'lltri';                             Data:'9722'),
    (Value:'ll';                                Data:'8810'),
    (Value:'lhblk';                             Data:'9604'),
    (Value:'lharul';                            Data:'10602'),
    (Value:'ljcy';                              Data:'1113'),
    (Value:'llcorner';                          Data:'8990'),
    (Value:'llarr';                             Data:'8647'),
    (Value:'llhard';                            Data:'10603'),
    (Value:'lnE';                               Data:'8808'),
    (Value:'lmoust';                            Data:'9136'),
    (Value:'lmidot';                            Data:'320'),
    (Value:'lmoustache';                        Data:'9136'),
    (Value:'lnapprox';                          Data:'10889'),
    (Value:'lnap';                              Data:'10889'),
    (Value:'lne';                               Data:'10887'),
    (Value:'longmapsto';                        Data:'10236'),
    (Value:'loarr';                             Data:'8701'),
    (Value:'lnsim';                             Data:'8934'),
    (Value:'lneqq';                             Data:'8808'),
    (Value:'loang';                             Data:'10220'),
    (Value:'longleftarrow';                     Data:'10229'),
    (Value:'lobrk';                             Data:'10214'),
    (Value:'longleftrightarrow';                Data:'10231'),
    (Value:'lopar';                             Data:'10629'),
    (Value:'looparrowleft';                     Data:'8619'),
    (Value:'longrightarrow';                    Data:'10230'),
    (Value:'looparrowright';                    Data:'8620'),
    (Value:'loplus';                            Data:'10797'),
    (Value:'lopf';                              Data:'120157'),
    (Value:'lotimes';                           Data:'10804'),
    (Value:'lowast';                            Data:'8727'),
    (Value:'lsimg';                             Data:'10895'),
    (Value:'lrhar';                             Data:'8651'),
    (Value:'lpar';                              Data:'40'),
    (Value:'lozenge';                           Data:'9674'),
    (Value:'loz';                               Data:'9674'),
    (Value:'lozf';                              Data:'10731'),
    (Value:'lrarr';                             Data:'8646'),
    (Value:'lparlt';                            Data:'10643'),
    (Value:'lrcorner';                          Data:'8991'),
    (Value:'lscr';                              Data:'120001'),
    (Value:'lrtri';                             Data:'8895'),
    (Value:'lrhard';                            Data:'10605'),
    (Value:'lsaquo';                            Data:'8249'),
    (Value:'lsim';                              Data:'8818'),
    (Value:'lsh';                               Data:'8624'),
    (Value:'lsime';                             Data:'10893'),
    (Value:'ltdot';                             Data:'8918'),
    (Value:'lstrok';                            Data:'322'),
    (Value:'lsquo';                             Data:'8216'),
    (Value:'lsqb';                              Data:'91'),
    (Value:'lsquor';                            Data:'8218'),
    (Value:'ltcc';                              Data:'10918'),
    (Value:'lt';                                Data:'60'),
    (Value:'ltcir';                             Data:'10873'),
    (Value:'ltquest';                           Data:'10875'),
    (Value:'ltimes';                            Data:'8905'),
    (Value:'lthree';                            Data:'8907'),
    (Value:'ltlarr';                            Data:'10614'),
    (Value:'ltri';                              Data:'9667'),
    (Value:'ltrPar';                            Data:'10646'),
    (Value:'ltrie';                             Data:'8884'),
    (Value:'ltrif';                             Data:'9666'),
    (Value:'notnivc';                           Data:'8957'),
    (Value:'nbumpe';                            Data:'8783,824'),
    (Value:'mnplus';                            Data:'8723'),
    (Value:'mcy';                               Data:'1084'),
    (Value:'maltese';                           Data:'10016'),
    (Value:'mDDot';                             Data:'8762'),
    (Value:'lvertneqq';                         Data:'8808,65024'),
    (Value:'luruhar';                           Data:'10598'),
    (Value:'lvnE';                              Data:'8808,65024'),
    (Value:'male';                              Data:'9794'),
    (Value:'macr';                              Data:'175'),
    (Value:'malt';                              Data:'10016'),
    (Value:'mapstoleft';                        Data:'8612'),
    (Value:'mapsto';                            Data:'8614'),
    (Value:'map';                               Data:'8614'),
    (Value:'mapstodown';                        Data:'8615'),
    (Value:'marker';                            Data:'9646'),
    (Value:'mapstoup';                          Data:'8613'),
    (Value:'mcomma';                            Data:'10793'),
    (Value:'midcir';                            Data:'10992'),
    (Value:'mho';                               Data:'8487'),
    (Value:'measuredangle';                     Data:'8737'),
    (Value:'mdash';                             Data:'8212'),
    (Value:'mfr';                               Data:'120106'),
    (Value:'mid';                               Data:'8739'),
    (Value:'micro';                             Data:'181'),
    (Value:'midast';                            Data:'42'),
    (Value:'minusb';                            Data:'8863'),
    (Value:'minus';                             Data:'45'),
    (Value:'middot';                            Data:'183'),
    (Value:'minus';                             Data:'8722'),
    (Value:'minusdu';                           Data:'10794'),
    (Value:'minusd';                            Data:'8760'),
    (Value:'mlcp';                              Data:'10971'),
    (Value:'mldr';                              Data:'8230'),
    (Value:'nLtv';                              Data:'8810,824'),
    (Value:'mumap';                             Data:'8888'),
    (Value:'mscr';                              Data:'120002'),
    (Value:'mopf';                              Data:'120158'),
    (Value:'models';                            Data:'8871'),
    (Value:'mp';                                Data:'8723'),
    (Value:'mu';                                Data:'956'),
    (Value:'mstpos';                            Data:'8766'),
    (Value:'multimap';                          Data:'8888'),
    (Value:'nLeftarrow';                        Data:'8653'),
    (Value:'nGt';                               Data:'8811,8402'),
    (Value:'nGg';                               Data:'8921,824'),
    (Value:'nGtv';                              Data:'8811,824'),
    (Value:'nLl';                               Data:'8920,824'),
    (Value:'nLeftrightarrow';                   Data:'8654'),
    (Value:'nLt';                               Data:'8810,8402'),
    (Value:'napE';                              Data:'10864,824'),
    (Value:'nabla';                             Data:'8711'),
    (Value:'nVDash';                            Data:'8879'),
    (Value:'nRightarrow';                       Data:'8655'),
    (Value:'nVdash';                            Data:'8878'),
    (Value:'nang';                              Data:'8736,8402'),
    (Value:'nacute';                            Data:'324'),
    (Value:'nap';                               Data:'8777'),
    (Value:'natur';                             Data:'9838'),
    (Value:'napos';                             Data:'329'),
    (Value:'napid';                             Data:'8779,824'),
    (Value:'napprox';                           Data:'8777'),
    (Value:'naturals';                          Data:'8469'),
    (Value:'natural';                           Data:'9838'),
    (Value:'nbsp';                              Data:'32'),
    (Value:'nbump';                             Data:'8782,824'),
    (Value:'ni';                                Data:'8715'),
    (Value:'nesear';                            Data:'10536'),
    (Value:'ndash';                             Data:'8211'),
    (Value:'ncong';                             Data:'8775'),
    (Value:'ncaron';                            Data:'328'),
    (Value:'ncap';                              Data:'10819'),
    (Value:'ncedil';                            Data:'326'),
    (Value:'ncup';                              Data:'10818'),
    (Value:'ncongdot';                          Data:'10861,824'),
    (Value:'ncy';                               Data:'1085'),
    (Value:'nearr';                             Data:'8599'),
    (Value:'neArr';                             Data:'8663'),
    (Value:'ne';                                Data:'8800'),
    (Value:'nearhk';                            Data:'10532'),
    (Value:'nedot';                             Data:'8784,824'),
    (Value:'nearrow';                           Data:'8599'),
    (Value:'nequiv';                            Data:'8802'),
    (Value:'ngeqq';                             Data:'8807,824'),
    (Value:'nfr';                               Data:'120107'),
    (Value:'nexist';                            Data:'8708'),
    (Value:'nesim';                             Data:'8770,824'),
    (Value:'nexists';                           Data:'8708'),
    (Value:'nge';                               Data:'8817'),
    (Value:'ngE';                               Data:'8807,824'),
    (Value:'ngeq';                              Data:'8817'),
    (Value:'ngt';                               Data:'8815'),
    (Value:'nges';                              Data:'10878,824'),
    (Value:'ngeqslant';                         Data:'10878,824'),
    (Value:'ngsim';                             Data:'8821'),
    (Value:'nhArr';                             Data:'8654'),
    (Value:'ngtr';                              Data:'8815'),
    (Value:'nharr';                             Data:'8622'),
    (Value:'nhpar';                             Data:'10994'),
    (Value:'nless';                             Data:'8814'),
    (Value:'nldr';                              Data:'8229'),
    (Value:'njcy';                              Data:'1114'),
    (Value:'nisd';                              Data:'8954'),
    (Value:'nis';                               Data:'8956'),
    (Value:'niv';                               Data:'8715'),
    (Value:'nlE';                               Data:'8806,824'),
    (Value:'nlArr';                             Data:'8653'),
    (Value:'nlarr';                             Data:'8602'),
    (Value:'nleq';                              Data:'8816'),
    (Value:'nleftarrow';                        Data:'8602'),
    (Value:'nle';                               Data:'8816'),
    (Value:'nleftrightarrow';                   Data:'8622'),
    (Value:'nleqslant';                         Data:'10877,824'),
    (Value:'nleqq';                             Data:'8806,824'),
    (Value:'nles';                              Data:'10877,824'),
    (Value:'notin';                             Data:'8713'),
    (Value:'nltrie';                            Data:'8940'),
    (Value:'nlt';                               Data:'8814'),
    (Value:'nlsim';                             Data:'8820'),
    (Value:'nltri';                             Data:'8938'),
    (Value:'nopf';                              Data:'120159'),
    (Value:'nmid';                              Data:'8740'),
    (Value:'not';                               Data:'172'),
    (Value:'notinvb';                           Data:'8951'),
    (Value:'notindot';                          Data:'8949,824'),
    (Value:'notinE';                            Data:'8953,824'),
    (Value:'notinva';                           Data:'8713'),
    (Value:'notni';                             Data:'8716'),
    (Value:'notinvc';                           Data:'8950'),
    (Value:'notniva';                           Data:'8716'),
    (Value:'notnivb';                           Data:'8958'),
    (Value:'nvrArr';                            Data:'10499'),
    (Value:'nsube';                             Data:'8840'),
    (Value:'nrtri';                             Data:'8939'),
    (Value:'npre';                              Data:'10927,824'),
    (Value:'npart';                             Data:'8706,824'),
    (Value:'nparallel';                         Data:'8742'),
    (Value:'npar';                              Data:'8742'),
    (Value:'nparsl';                            Data:'11005,8421'),
    (Value:'npr';                               Data:'8832'),
    (Value:'npolint';                           Data:'10772'),
    (Value:'nprcue';                            Data:'8928'),
    (Value:'nrarr';                             Data:'8603'),
    (Value:'npreceq';                           Data:'10927,824'),
    (Value:'nprec';                             Data:'8832'),
    (Value:'nrArr';                             Data:'8655'),
    (Value:'nrarrw';                            Data:'8605,824'),
    (Value:'nrarrc';                            Data:'10547,824'),
    (Value:'nrightarrow';                       Data:'8603'),
    (Value:'nsim';                              Data:'8769'),
    (Value:'nsce';                              Data:'10928,824'),
    (Value:'nsc';                               Data:'8833'),
    (Value:'nrtrie';                            Data:'8941'),
    (Value:'nsccue';                            Data:'8929'),
    (Value:'nshortmid';                         Data:'8740'),
    (Value:'nscr';                              Data:'120003'),
    (Value:'nshortparallel';                    Data:'8742'),
    (Value:'nspar';                             Data:'8742'),
    (Value:'nsimeq';                            Data:'8772'),
    (Value:'nsime';                             Data:'8772'),
    (Value:'nsmid';                             Data:'8740'),
    (Value:'nsqsupe';                           Data:'8931'),
    (Value:'nsqsube';                           Data:'8930'),
    (Value:'nsub';                              Data:'8836'),
    (Value:'nsubE';                             Data:'10949,824'),
    (Value:'ntrianglelefteq';                   Data:'8940'),
    (Value:'nsupe';                             Data:'8841'),
    (Value:'nsucc';                             Data:'8833'),
    (Value:'nsubseteq';                         Data:'8840'),
    (Value:'nsubset';                           Data:'8834,8402'),
    (Value:'nsubseteqq';                        Data:'10949,824'),
    (Value:'nsup';                              Data:'8837'),
    (Value:'nsucceq';                           Data:'10928,824'),
    (Value:'nsupE';                             Data:'10950,824'),
    (Value:'ntgl';                              Data:'8825'),
    (Value:'nsupseteq';                         Data:'8841'),
    (Value:'nsupset';                           Data:'8835,8402'),
    (Value:'nsupseteqq';                        Data:'10950,824'),
    (Value:'ntlg';                              Data:'8824'),
    (Value:'ntilde';                            Data:'241'),
    (Value:'ntriangleleft';                     Data:'8938'),
    (Value:'nvap';                              Data:'8781,8402'),
    (Value:'num';                               Data:'35'),
    (Value:'ntrianglerighteq';                  Data:'8941'),
    (Value:'ntriangleright';                    Data:'8939'),
    (Value:'nu';                                Data:'957'),
    (Value:'nvDash';                            Data:'8877'),
    (Value:'numero';                            Data:'8470'),
    (Value:'nvHarr';                            Data:'10500'),
    (Value:'nvinfin';                           Data:'10718'),
    (Value:'nvge';                              Data:'8805,8402'),
    (Value:'nvdash';                            Data:'8876'),
    (Value:'nvgt';                              Data:'62,8402'),
    (Value:'nvle';                              Data:'8804,8402'),
    (Value:'nvlArr';                            Data:'10498'),
    (Value:'nvlt';                              Data:'60,8402'),
    (Value:'nvltrie';                           Data:'8884,8402'),
    (Value:'omacr';                             Data:'333'),
    (Value:'odiv';                              Data:'10808'),
    (Value:'oS';                                Data:'9416'),
    (Value:'nwarhk';                            Data:'10531'),
    (Value:'nvsim';                             Data:'8764,8402'),
    (Value:'nvrtrie';                           Data:'8885,8402'),
    (Value:'nwArr';                             Data:'8662'),
    (Value:'nwarrow';                           Data:'8598'),
    (Value:'nwarr';                             Data:'8598'),
    (Value:'nwnear';                            Data:'10535'),
    (Value:'ocirc';                             Data:'244'),
    (Value:'oast';                              Data:'8859'),
    (Value:'oacute';                            Data:'243'),
    (Value:'ocir';                              Data:'8858'),
    (Value:'odash';                             Data:'8861'),
    (Value:'ocy';                               Data:'1086'),
    (Value:'odblac';                            Data:'337'),
    (Value:'ogt';                               Data:'10689'),
    (Value:'ofcir';                             Data:'10687'),
    (Value:'odsold';                            Data:'10684'),
    (Value:'odot';                              Data:'8857'),
    (Value:'oelig';                             Data:'339'),
    (Value:'ogon';                              Data:'731'),
    (Value:'ofr';                               Data:'120108'),
    (Value:'ograve';                            Data:'242'),
    (Value:'olarr';                             Data:'8634'),
    (Value:'ohm';                               Data:'937'),
    (Value:'ohbar';                             Data:'10677'),
    (Value:'oint';                              Data:'8750'),
    (Value:'olcross';                           Data:'10683'),
    (Value:'olcir';                             Data:'10686'),
    (Value:'oline';                             Data:'8254'),
    (Value:'olt';                               Data:'10688'),
    (Value:'origof';                            Data:'8886'),
    (Value:'oplus';                             Data:'8853'),
    (Value:'ominus';                            Data:'8854'),
    (Value:'omicron';                           Data:'959'),
    (Value:'omega';                             Data:'969'),
    (Value:'omid';                              Data:'10678'),
    (Value:'opar';                              Data:'10679'),
    (Value:'oopf';                              Data:'120160'),
    (Value:'operp';                             Data:'10681'),
    (Value:'order';                             Data:'8500'),
    (Value:'orarr';                             Data:'8635'),
    (Value:'or';                                Data:'8744'),
    (Value:'ord';                               Data:'10845'),
    (Value:'ordf';                              Data:'170'),
    (Value:'orderof';                           Data:'8500'),
    (Value:'ordm';                              Data:'186'),
    (Value:'otimes';                            Data:'8855'),
    (Value:'oscr';                              Data:'8500'),
    (Value:'orslope';                           Data:'10839'),
    (Value:'oror';                              Data:'10838'),
    (Value:'orv';                               Data:'10843'),
    (Value:'osol';                              Data:'8856'),
    (Value:'oslash';                            Data:'248'),
    (Value:'otilde';                            Data:'245'),
    (Value:'par';                               Data:'8741'),
    (Value:'ouml';                              Data:'246'),
    (Value:'otimesas';                          Data:'10806'),
    (Value:'ovbar';                             Data:'9021'),
    (Value:'parallel';                          Data:'8741'),
    (Value:'para';                              Data:'182'),
    (Value:'parsim';                            Data:'10995'),
    (Value:'parsl';                             Data:'11005'),
    (Value:'strns';                             Data:'175'),
    (Value:'rightharpoondown';                  Data:'8641'),
    (Value:'quatint';                           Data:'10774'),
    (Value:'prE';                               Data:'10931'),
    (Value:'planckh';                           Data:'8462'),
    (Value:'phi';                               Data:'966'),
    (Value:'permil';                            Data:'8240'),
    (Value:'percnt';                            Data:'37'),
    (Value:'pcy';                               Data:'1087'),
    (Value:'period';                            Data:'46'),
    (Value:'pertenk';                           Data:'8241'),
    (Value:'perp';                              Data:'8869'),
    (Value:'pfr';                               Data:'120109'),
    (Value:'pi';                                Data:'960'),
    (Value:'phmmat';                            Data:'8499'),
    (Value:'phiv';                              Data:'966'),
    (Value:'phone';                             Data:'9742'),
    (Value:'piv';                               Data:'982'),
    (Value:'pitchfork';                         Data:'8916'),
    (Value:'planck';                            Data:'8463'),
    (Value:'pluse';                             Data:'10866'),
    (Value:'plusb';                             Data:'8862'),
    (Value:'plus';                              Data:'43'),
    (Value:'plankv';                            Data:'8463'),
    (Value:'plusacir';                          Data:'10787'),
    (Value:'plusdo';                            Data:'8724'),
    (Value:'pluscir';                           Data:'10786'),
    (Value:'plusdu';                            Data:'10789'),
    (Value:'pm';                                Data:'177'),
    (Value:'plussim';                           Data:'10790'),
    (Value:'plusmn';                            Data:'177'),
    (Value:'plustwo';                           Data:'10791'),
    (Value:'popf';                              Data:'120161'),
    (Value:'pointint';                          Data:'10773'),
    (Value:'pound';                             Data:'163'),
    (Value:'pr';                                Data:'8826'),
    (Value:'prnsim';                            Data:'8936'),
    (Value:'precnapprox';                       Data:'10937'),
    (Value:'prec';                              Data:'8826'),
    (Value:'prcue';                             Data:'8828'),
    (Value:'prap';                              Data:'10935'),
    (Value:'pre';                               Data:'10927'),
    (Value:'preccurlyeq';                       Data:'8828'),
    (Value:'precapprox';                        Data:'10935'),
    (Value:'preceq';                            Data:'10927'),
    (Value:'prime';                             Data:'8242'),
    (Value:'precnsim';                          Data:'8936'),
    (Value:'precneqq';                          Data:'10933'),
    (Value:'precsim';                           Data:'8830'),
    (Value:'prnE';                              Data:'10933'),
    (Value:'primes';                            Data:'8473'),
    (Value:'prnap';                             Data:'10937'),
    (Value:'prurel';                            Data:'8880'),
    (Value:'profsurf';                          Data:'8979'),
    (Value:'profalar';                          Data:'9006'),
    (Value:'prod';                              Data:'8719'),
    (Value:'profline';                          Data:'8978'),
    (Value:'propto';                            Data:'8733'),
    (Value:'prop';                              Data:'8733'),
    (Value:'prsim';                             Data:'8830'),
    (Value:'qint';                              Data:'10764'),
    (Value:'psi';                               Data:'968'),
    (Value:'pscr';                              Data:'120005'),
    (Value:'qfr';                               Data:'120110'),
    (Value:'qprime';                            Data:'8279'),
    (Value:'qopf';                              Data:'120162'),
    (Value:'qscr';                              Data:'120006'),
    (Value:'quaternions';                       Data:'8461'),
    (Value:'rbarr';                             Data:'10509'),
    (Value:'rangle';                            Data:'12297'),
    (Value:'rHar';                              Data:'10596'),
    (Value:'rAarr';                             Data:'8667'),
    (Value:'questeq';                           Data:'8799'),
    (Value:'quest';                             Data:'63'),
    (Value:'quot';                              Data:'34'),
    (Value:'rAtail';                            Data:'10524'),
    (Value:'rArr';                              Data:'8658'),
    (Value:'rBarr';                             Data:'10511'),
    (Value:'raemptyv';                          Data:'10675'),
    (Value:'racute';                            Data:'341'),
    (Value:'race';                              Data:'8765,817'),
    (Value:'radic';                             Data:'8730'),
    (Value:'rangd';                             Data:'10642'),
    (Value:'rang';                              Data:'12297'),
    (Value:'range';                             Data:'10661'),
    (Value:'rarrhk';                            Data:'8618'),
    (Value:'rarrb';                             Data:'8677'),
    (Value:'rarr';                              Data:'8594'),
    (Value:'raquo';                             Data:'187'),
    (Value:'rarrap';                            Data:'10613'),
    (Value:'rarrc';                             Data:'10547'),
    (Value:'rarrbfs';                           Data:'10528'),
    (Value:'rarrfs';                            Data:'10526'),
    (Value:'rarrtl';                            Data:'8611'),
    (Value:'rarrpl';                            Data:'10565'),
    (Value:'rarrlp';                            Data:'8620'),
    (Value:'rarrsim';                           Data:'10612'),
    (Value:'ratail';                            Data:'10522'),
    (Value:'rarrw';                             Data:'8605'),
    (Value:'ratio';                             Data:'8758'),
    (Value:'rationals';                         Data:'8474'),
    (Value:'rdsh';                              Data:'8627'),
    (Value:'rcedil';                            Data:'343'),
    (Value:'rbrke';                             Data:'10636'),
    (Value:'rbrace';                            Data:'125'),
    (Value:'rbbrk';                             Data:'10099'),
    (Value:'rbrack';                            Data:'93'),
    (Value:'rbrkslu';                           Data:'10640'),
    (Value:'rbrksld';                           Data:'10638'),
    (Value:'rcaron';                            Data:'345'),
    (Value:'rdca';                              Data:'10551'),
    (Value:'rcub';                              Data:'125'),
    (Value:'rceil';                             Data:'8969'),
    (Value:'rcy';                               Data:'1088'),
    (Value:'rdquo';                             Data:'8221'),
    (Value:'rdldhar';                           Data:'10601'),
    (Value:'rdquor';                            Data:'8221'),
    (Value:'rfloor';                            Data:'8971'),
    (Value:'reals';                             Data:'8477'),
    (Value:'realine';                           Data:'8475'),
    (Value:'real';                              Data:'8476'),
    (Value:'realpart';                          Data:'8476'),
    (Value:'reg';                               Data:'174'),
    (Value:'rect';                              Data:'9645'),
    (Value:'rfisht';                            Data:'10621'),
    (Value:'rharul';                            Data:'10604'),
    (Value:'rhard';                             Data:'8641'),
    (Value:'rfr';                               Data:'120111'),
    (Value:'rharu';                             Data:'8640'),
    (Value:'rhov';                              Data:'1009'),
    (Value:'rho';                               Data:'961'),
    (Value:'rightarrow';                        Data:'8594'),
    (Value:'rightarrowtail';                    Data:'8611'),
    (Value:'setmn';                             Data:'8726'),
    (Value:'rtri';                              Data:'9657'),
    (Value:'robrk';                             Data:'10215'),
    (Value:'risingdotseq';                      Data:'8787'),
    (Value:'rightrightarrows';                  Data:'8649'),
    (Value:'rightleftarrows';                   Data:'8644'),
    (Value:'rightharpoonup';                    Data:'8640'),
    (Value:'rightleftharpoons';                 Data:'8652'),
    (Value:'rightthreetimes';                   Data:'8908'),
    (Value:'rightsquigarrow';                   Data:'8605'),
    (Value:'ring';                              Data:'730'),
    (Value:'rmoustache';                        Data:'9137'),
    (Value:'rlhar';                             Data:'8652'),
    (Value:'rlarr';                             Data:'8644'),
    (Value:'rmoust';                            Data:'9137'),
    (Value:'roang';                             Data:'10221'),
    (Value:'rnmid';                             Data:'10990'),
    (Value:'roarr';                             Data:'8702'),
    (Value:'rrarr';                             Data:'8649'),
    (Value:'rotimes';                           Data:'10805'),
    (Value:'ropf';                              Data:'120163'),
    (Value:'ropar';                             Data:'10630'),
    (Value:'roplus';                            Data:'10798'),
    (Value:'rpargt';                            Data:'10644'),
    (Value:'rpar';                              Data:'41'),
    (Value:'rppolint';                          Data:'10770'),
    (Value:'rsqb';                              Data:'93'),
    (Value:'rscr';                              Data:'120007'),
    (Value:'rsaquo';                            Data:'8250'),
    (Value:'rsh';                               Data:'8625'),
    (Value:'rsquor';                            Data:'8217'),
    (Value:'rsquo';                             Data:'8217'),
    (Value:'rthree';                            Data:'8908'),
    (Value:'rtimes';                            Data:'8906'),
    (Value:'scnE';                              Data:'10934'),
    (Value:'sc';                                Data:'8827'),
    (Value:'ruluhar';                           Data:'10600'),
    (Value:'rtrif';                             Data:'9656'),
    (Value:'rtrie';                             Data:'8885'),
    (Value:'rtriltri';                          Data:'10702'),
    (Value:'sacute';                            Data:'347'),
    (Value:'rx';                                Data:'8478'),
    (Value:'sbquo';                             Data:'8218'),
    (Value:'sccue';                             Data:'8829'),
    (Value:'scap';                              Data:'10936'),
    (Value:'scE';                               Data:'10932'),
    (Value:'scaron';                            Data:'353'),
    (Value:'scedil';                            Data:'351'),
    (Value:'sce';                               Data:'10928'),
    (Value:'scirc';                             Data:'349'),
    (Value:'sdote';                             Data:'10854'),
    (Value:'scsim';                             Data:'8831'),
    (Value:'scnsim';                            Data:'8937'),
    (Value:'scnap';                             Data:'10938'),
    (Value:'scpolint';                          Data:'10771'),
    (Value:'sdot';                              Data:'8901'),
    (Value:'scy';                               Data:'1089'),
    (Value:'sdotb';                             Data:'8865'),
    (Value:'searrow';                           Data:'8600'),
    (Value:'searhk';                            Data:'10533'),
    (Value:'seArr';                             Data:'8664'),
    (Value:'searr';                             Data:'8600'),
    (Value:'semi';                              Data:'59'),
    (Value:'sect';                              Data:'167'),
    (Value:'seswar';                            Data:'10537'),
    (Value:'setminus';                          Data:'8726'),
    (Value:'softcy';                            Data:'1100'),
    (Value:'simeq';                             Data:'8771'),
    (Value:'shortparallel';                     Data:'8741'),
    (Value:'sharp';                             Data:'9839'),
    (Value:'sfr';                               Data:'120112'),
    (Value:'sext';                              Data:'10038'),
    (Value:'sfrown';                            Data:'8994'),
    (Value:'shcy';                              Data:'1096'),
    (Value:'shchcy';                            Data:'1097'),
    (Value:'shortmid';                          Data:'8739'),
    (Value:'sigmav';                            Data:'962'),
    (Value:'sigma';                             Data:'963'),
    (Value:'shy';                               Data:'173'),
    (Value:'sigmaf';                            Data:'962'),
    (Value:'simdot';                            Data:'10858'),
    (Value:'sim';                               Data:'8764'),
    (Value:'sime';                              Data:'8771'),
    (Value:'slarr';                             Data:'8592'),
    (Value:'simlE';                             Data:'10911'),
    (Value:'simgE';                             Data:'10912'),
    (Value:'simg';                              Data:'10910'),
    (Value:'siml';                              Data:'10909'),
    (Value:'simplus';                           Data:'10788'),
    (Value:'simne';                             Data:'8774'),
    (Value:'simrarr';                           Data:'10610'),
    (Value:'smid';                              Data:'8739'),
    (Value:'smashp';                            Data:'10803'),
    (Value:'smallsetminus';                     Data:'8726'),
    (Value:'smeparsl';                          Data:'10724'),
    (Value:'smt';                               Data:'10922'),
    (Value:'smile';                             Data:'8995'),
    (Value:'smte';                              Data:'10924'),
    (Value:'smtes';                             Data:'10924,65024'),
    (Value:'sqsup';                             Data:'8848'),
    (Value:'sqcap';                             Data:'8851'),
    (Value:'sopf';                              Data:'120164'),
    (Value:'solb';                              Data:'10692'),
    (Value:'sol';                               Data:'47'),
    (Value:'solbar';                            Data:'9023'),
    (Value:'spadesuit';                         Data:'9824'),
    (Value:'spades';                            Data:'9824'),
    (Value:'spar';                              Data:'8741'),
    (Value:'sqsub';                             Data:'8847'),
    (Value:'sqcup';                             Data:'8852'),
    (Value:'sqcaps';                            Data:'8851,65024'),
    (Value:'sqcups';                            Data:'8852,65024'),
    (Value:'sqsubset';                          Data:'8847'),
    (Value:'sqsube';                            Data:'8849'),
    (Value:'sqsubseteq';                        Data:'8849'),
    (Value:'srarr';                             Data:'8594'),
    (Value:'squ';                               Data:'9633'),
    (Value:'sqsupset';                          Data:'8848'),
    (Value:'sqsupe';                            Data:'8850'),
    (Value:'sqsupseteq';                        Data:'8850'),
    (Value:'squarf';                            Data:'9642'),
    (Value:'square';                            Data:'9633'),
    (Value:'squf';                              Data:'9642'),
    (Value:'sstarf';                            Data:'8902'),
    (Value:'ssetmn';                            Data:'8726'),
    (Value:'sscr';                              Data:'120008'),
    (Value:'ssmile';                            Data:'8995'),
    (Value:'starf';                             Data:'9733'),
    (Value:'star';                              Data:'9734'),
    (Value:'straightepsilon';                   Data:'1013'),
    (Value:'straightphi';                       Data:'981'),
    (Value:'ulcorner';                          Data:'8988'),
    (Value:'telrec';                            Data:'8981'),
    (Value:'supE';                              Data:'10950'),
    (Value:'subsim';                            Data:'10951'),
    (Value:'subne';                             Data:'8842'),
    (Value:'sube';                              Data:'8838'),
    (Value:'subE';                              Data:'10949'),
    (Value:'sub';                               Data:'8834'),
    (Value:'subdot';                            Data:'10941'),
    (Value:'submult';                           Data:'10945'),
    (Value:'subedot';                           Data:'10947'),
    (Value:'subnE';                             Data:'10955'),
    (Value:'subseteq';                          Data:'8838'),
    (Value:'subrarr';                           Data:'10617'),
    (Value:'subplus';                           Data:'10943'),
    (Value:'subset';                            Data:'8834'),
    (Value:'subsetneq';                         Data:'8842'),
    (Value:'subseteqq';                         Data:'10949'),
    (Value:'subsetneqq';                        Data:'10955'),
    (Value:'succneqq';                          Data:'10934'),
    (Value:'succapprox';                        Data:'10936'),
    (Value:'subsup';                            Data:'10963'),
    (Value:'subsub';                            Data:'10965'),
    (Value:'succ';                              Data:'8827'),
    (Value:'succeq';                            Data:'10928'),
    (Value:'succcurlyeq';                       Data:'8829'),
    (Value:'succnapprox';                       Data:'10938'),
    (Value:'sung';                              Data:'9834'),
    (Value:'succsim';                           Data:'8831'),
    (Value:'succnsim';                          Data:'8937'),
    (Value:'sum';                               Data:'8721'),
    (Value:'sup1';                              Data:'185'),
    (Value:'sup';                               Data:'8835'),
    (Value:'sup2';                              Data:'178'),
    (Value:'sup3';                              Data:'179'),
    (Value:'supsetneqq';                        Data:'10956'),
    (Value:'supmult';                           Data:'10946'),
    (Value:'supedot';                           Data:'10948'),
    (Value:'supdsub';                           Data:'10968'),
    (Value:'supdot';                            Data:'10942'),
    (Value:'supe';                              Data:'8839'),
    (Value:'suphsub';                           Data:'10967'),
    (Value:'suphsol';                           Data:'10185'),
    (Value:'suplarr';                           Data:'10619'),
    (Value:'supset';                            Data:'8835'),
    (Value:'supne';                             Data:'8843'),
    (Value:'supnE';                             Data:'10956'),
    (Value:'supplus';                           Data:'10944'),
    (Value:'supseteqq';                         Data:'10950'),
    (Value:'supseteq';                          Data:'8839'),
    (Value:'supsetneq';                         Data:'8843'),
    (Value:'swnwar';                            Data:'10538'),
    (Value:'swArr';                             Data:'8665'),
    (Value:'supsub';                            Data:'10964'),
    (Value:'supsim';                            Data:'10952'),
    (Value:'supsup';                            Data:'10966'),
    (Value:'swarr';                             Data:'8601'),
    (Value:'swarhk';                            Data:'10534'),
    (Value:'swarrow';                           Data:'8601'),
    (Value:'tbrk';                              Data:'9140'),
    (Value:'target';                            Data:'8982'),
    (Value:'szlig';                             Data:'223'),
    (Value:'tau';                               Data:'964'),
    (Value:'tcedil';                            Data:'355'),
    (Value:'tcaron';                            Data:'357'),
    (Value:'tcy';                               Data:'1090'),
    (Value:'tdot';                              Data:'8411'),
    (Value:'trianglerighteq';                   Data:'8885'),
    (Value:'timesd';                            Data:'10800'),
    (Value:'thicksim';                          Data:'8764'),
    (Value:'theta';                             Data:'952'),
    (Value:'there4';                            Data:'8756'),
    (Value:'tfr';                               Data:'120113'),
    (Value:'therefore';                         Data:'8756'),
    (Value:'thetav';                            Data:'977'),
    (Value:'thetasym';                          Data:'977'),
    (Value:'thickapprox';                       Data:'8776'),
    (Value:'tilde';                             Data:'732'),
    (Value:'thksim';                            Data:'8764'),
    (Value:'thkap';                             Data:'8776'),
    (Value:'thorn';                             Data:'254'),
    (Value:'timesb';                            Data:'8864'),
    (Value:'times';                             Data:'215'),
    (Value:'timesbar';                          Data:'10801'),
    (Value:'tosa';                              Data:'10537'),
    (Value:'topbot';                            Data:'9014'),
    (Value:'toea';                              Data:'10536'),
    (Value:'tint';                              Data:'8749'),
    (Value:'top';                               Data:'8868'),
    (Value:'topf';                              Data:'120165'),
    (Value:'topcir';                            Data:'10993'),
    (Value:'topfork';                           Data:'10970'),
    (Value:'triangledown';                      Data:'9663'),
    (Value:'trade';                             Data:'8482'),
    (Value:'tprime';                            Data:'8244'),
    (Value:'triangle';                          Data:'9653'),
    (Value:'trianglelefteq';                    Data:'8884'),
    (Value:'triangleleft';                      Data:'9667'),
    (Value:'triangleq';                         Data:'8796'),
    (Value:'triangleright';                     Data:'9657'),
    (Value:'uHar';                              Data:'10595'),
    (Value:'tscr';                              Data:'120009'),
    (Value:'triplus';                           Data:'10809'),
    (Value:'trie';                              Data:'8796'),
    (Value:'tridot';                            Data:'9708'),
    (Value:'triminus';                          Data:'10810'),
    (Value:'tritime';                           Data:'10811'),
    (Value:'trisb';                             Data:'10701'),
    (Value:'trpezium';                          Data:'9186'),
    (Value:'twixt';                             Data:'8812'),
    (Value:'tshcy';                             Data:'1115'),
    (Value:'tscy';                              Data:'1094'),
    (Value:'tstrok';                            Data:'359'),
    (Value:'twoheadrightarrow';                 Data:'8608'),
    (Value:'twoheadleftarrow';                  Data:'8606'),
    (Value:'uArr';                              Data:'8657'),
    (Value:'udblac';                            Data:'369'),
    (Value:'ubreve';                            Data:'365'),
    (Value:'uarr';                              Data:'8593'),
    (Value:'uacute';                            Data:'250'),
    (Value:'ubrcy';                             Data:'1118'),
    (Value:'ucy';                               Data:'1091'),
    (Value:'ucirc';                             Data:'251'),
    (Value:'udarr';                             Data:'8645'),
    (Value:'ugrave';                            Data:'249'),
    (Value:'ufisht';                            Data:'10622'),
    (Value:'udhar';                             Data:'10606'),
    (Value:'ufr';                               Data:'120114'),
    (Value:'uharr';                             Data:'8638'),
    (Value:'uharl';                             Data:'8639'),
    (Value:'uhblk';                             Data:'9600'),
    (Value:'ulcorn';                            Data:'8988'),
    (Value:'vrtri';                             Data:'8883'),
    (Value:'vangrt';                            Data:'10652'),
    (Value:'urcorn';                            Data:'8989'),
    (Value:'updownarrow';                       Data:'8597'),
    (Value:'uml';                               Data:'168'),
    (Value:'ultri';                             Data:'9720'),
    (Value:'ulcrop';                            Data:'8975'),
    (Value:'umacr';                             Data:'363'),
    (Value:'uopf';                              Data:'120166'),
    (Value:'uogon';                             Data:'371'),
    (Value:'uparrow';                           Data:'8593'),
    (Value:'upsi';                              Data:'965'),
    (Value:'upharpoonright';                    Data:'8638'),
    (Value:'upharpoonleft';                     Data:'8639'),
    (Value:'uplus';                             Data:'8846'),
    (Value:'upsilon';                           Data:'965'),
    (Value:'upsih';                             Data:'978'),
    (Value:'upuparrows';                        Data:'8648'),
    (Value:'utri';                              Data:'9653'),
    (Value:'urtri';                             Data:'9721'),
    (Value:'urcrop';                            Data:'8974'),
    (Value:'urcorner';                          Data:'8989'),
    (Value:'uring';                             Data:'367'),
    (Value:'utdot';                             Data:'8944'),
    (Value:'uscr';                              Data:'120010'),
    (Value:'utilde';                            Data:'361'),
    (Value:'uwangle';                           Data:'10663'),
    (Value:'uuarr';                             Data:'8648'),
    (Value:'utrif';                             Data:'9652'),
    (Value:'uuml';                              Data:'252'),
    (Value:'vBar';                              Data:'10984'),
    (Value:'vArr';                              Data:'8661'),
    (Value:'vBarv';                             Data:'10985'),
    (Value:'vDash';                             Data:'8872'),
    (Value:'vartriangleright';                  Data:'8883'),
    (Value:'varrho';                            Data:'1009'),
    (Value:'varphi';                            Data:'966'),
    (Value:'varkappa';                          Data:'1008'),
    (Value:'varepsilon';                        Data:'1013'),
    (Value:'varnothing';                        Data:'8709'),
    (Value:'varpropto';                         Data:'8733'),
    (Value:'varpi';                             Data:'982'),
    (Value:'varr';                              Data:'8597'),
    (Value:'varsupsetneq';                      Data:'8843,65024'),
    (Value:'varsubsetneq';                      Data:'8842,65024'),
    (Value:'varsigma';                          Data:'962'),
    (Value:'varsubsetneqq';                     Data:'10955,65024'),
    (Value:'vartheta';                          Data:'977'),
    (Value:'varsupsetneqq';                     Data:'10956,65024'),
    (Value:'vartriangleleft';                   Data:'8882'),
    (Value:'verbar';                            Data:'166'),
    (Value:'veebar';                            Data:'8891'),
    (Value:'vdash';                             Data:'8866'),
    (Value:'vcy';                               Data:'1074'),
    (Value:'vee';                               Data:'8744'),
    (Value:'vellip';                            Data:'8942'),
    (Value:'veeeq';                             Data:'8794'),
    (Value:'verbar';                            Data:'124'),
    (Value:'vltri';                             Data:'8882'),
    (Value:'vert';                              Data:'166'),
    (Value:'vert';                              Data:'124'),
    (Value:'vfr';                               Data:'120115'),
    (Value:'vnsup';                             Data:'8835,8402'),
    (Value:'vnsub';                             Data:'8834,8402'),
    (Value:'vopf';                              Data:'120167'),
    (Value:'vprop';                             Data:'8733'),
    (Value:'xotime';                            Data:'10754'),
    (Value:'wreath';                            Data:'8768'),
    (Value:'wedbar';                            Data:'10847'),
    (Value:'vsupnE';                            Data:'10956,65024'),
    (Value:'vsubnE';                            Data:'10955,65024'),
    (Value:'vscr';                              Data:'120011'),
    (Value:'vsubne';                            Data:'8842,65024'),
    (Value:'vzigzag';                           Data:'10650'),
    (Value:'vsupne';                            Data:'8843,65024'),
    (Value:'wcirc';                             Data:'373'),
    (Value:'wfr';                               Data:'120116'),
    (Value:'wedgeq';                            Data:'8793'),
    (Value:'wedge';                             Data:'8743'),
    (Value:'weierp';                            Data:'8472'),
    (Value:'wp';                                Data:'8472'),
    (Value:'wopf';                              Data:'120168'),
    (Value:'wr';                                Data:'8768'),
    (Value:'xharr';                             Data:'10231'),
    (Value:'xcup';                              Data:'8899'),
    (Value:'xcap';                              Data:'8898'),
    (Value:'wscr';                              Data:'120012'),
    (Value:'xcirc';                             Data:'9711'),
    (Value:'xfr';                               Data:'120117'),
    (Value:'xdtri';                             Data:'9661'),
    (Value:'xhArr';                             Data:'10234'),
    (Value:'xmap';                              Data:'10236'),
    (Value:'xlArr';                             Data:'10232'),
    (Value:'xi';                                Data:'958'),
    (Value:'xlarr';                             Data:'10229'),
    (Value:'xodot';                             Data:'10752'),
    (Value:'xnis';                              Data:'8955'),
    (Value:'xopf';                              Data:'120169'),
    (Value:'xoplus';                            Data:'10753'),
    (Value:'yopf';                              Data:'120170'),
    (Value:'xwedge';                            Data:'8896'),
    (Value:'xsqcup';                            Data:'10758'),
    (Value:'xrarr';                             Data:'10230'),
    (Value:'xrArr';                             Data:'10233'),
    (Value:'xscr';                              Data:'120013'),
    (Value:'xutri';                             Data:'9651'),
    (Value:'xuplus';                            Data:'10756'),
    (Value:'xvee';                              Data:'8897'),
    (Value:'ycy';                               Data:'1099'),
    (Value:'yacy';                              Data:'1103'),
    (Value:'yacute';                            Data:'253'),
    (Value:'ycirc';                             Data:'375'),
    (Value:'yfr';                               Data:'120118'),
    (Value:'yen';                               Data:'165'),
    (Value:'yicy';                              Data:'1111'),
    (Value:'zeetrf';                            Data:'8488'),
    (Value:'zacute';                            Data:'378'),
    (Value:'yucy';                              Data:'1102'),
    (Value:'yscr';                              Data:'120014'),
    (Value:'yuml';                              Data:'255'),
    (Value:'zcy';                               Data:'1079'),
    (Value:'zcaron';                            Data:'382'),
    (Value:'zdot';                              Data:'380'),
    (Value:'zigrarr';                           Data:'8669'),
    (Value:'zfr';                               Data:'120119'),
    (Value:'zeta';                              Data:'950'),
    (Value:'zhcy';                              Data:'1078'),
    (Value:'zscr';                              Data:'120015'),
    (Value:'zopf';                              Data:'120171'),
    (Value:'zwj';                               Data:'8205'),
    (Value:'zwnj';                              Data:'8204')
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

procedure HTML2UTFdata; assembler; 
asm
	db 'gopf',0,                           '120152',0
	db 'Uopf',0,                           '120140',0
	db 'LessFullEqual',0,                  '8806',0
	db 'EmptySmallSquare',0,               '9723',0
	db 'CupCap',0,                         '8781',0
	db 'CHcy',0,                           '1063',0
	db 'Ascr',0,                           '119964',0
	db 'Agrave',0,                         '192',0
	db 'Abreve',0,                         '258',0
	db 'AMP',0,                            '38',0
	db 'AElig',0,                          '198',0
	db 'Aacute',0,                         '193',0
	db 'Acy',0,                            '1040',0
	db 'Acirc',0,                          '194',0
	db 'Afr',0,                            '120068',0
	db 'Aogon',0,                          '260',0
	db 'Amacr',0,                          '256',0
	db 'Alpha',0,                          '913',0
	db 'And',0,                            '10835',0
	db 'ApplyFunction',0,                  '8289',0
	db 'Aopf',0,                           '120120',0
	db 'Aring',0,                          '197',0
	db 'Because',0,                        '8757',0
	db 'Backslash',0,                      '8726',0
	db 'Atilde',0,                         '195',0
	db 'Assign',0,                         '8788',0
	db 'Auml',0,                           '196',0
	db 'Barwed',0,                         '8966',0
	db 'Barv',0,                           '10983',0
	db 'Bcy',0,                            '1041',0
	db 'Bopf',0,                           '120121',0
	db 'Beta',0,                           '914',0
	db 'Bernoullis',0,                     '8492',0
	db 'Bfr',0,                            '120069',0
	db 'Bscr',0,                           '8492',0
	db 'Breve',0,                          '728',0
	db 'Bumpeq',0,                         '8782',0
	db 'CircleMinus',0,                    '8854',0
	db 'Ccirc',0,                          '264',0
	db 'CapitalDifferentialD',0,           '8517',0
	db 'Cacute',0,                         '262',0
	db 'COPY',0,                           '169',0
	db 'Cap',0,                            '8914',0
	db 'Ccaron',0,                         '268',0
	db 'Cayleys',0,                        '8493',0
	db 'Ccedil',0,                         '199',0
	db 'CenterDot',0,                      '183',0
	db 'Cdot',0,                           '266',0
	db 'Cconint',0,                        '8752',0
	db 'Cedilla',0,                        '184',0
	db 'Chi',0,                            '935',0
	db 'Cfr',0,                            '8493',0
	db 'CircleDot',0,                      '8857',0
	db 'Congruent',0,                      '8801',0
	db 'CloseCurlyDoubleQuote',0,          '8221',0
	db 'CircleTimes',0,                    '8855',0
	db 'CirclePlus',0,                     '8853',0
	db 'ClockwiseContourIntegral',0,       '8754',0
	db 'Colon',0,                          '8759',0
	db 'CloseCurlyQuote',0,                '8217',0
	db 'Colone',0,                         '10868',0
	db 'Coproduct',0,                      '8720',0
	db 'ContourIntegral',0,                '8750',0
	db 'Conint',0,                         '8751',0
	db 'Copf',0,                           '8450',0
	db 'Cross',0,                          '10799',0
	db 'CounterClockwiseContourIntegral',0,'8755',0
	db 'Cscr',0,                           '119966',0
	db 'Cup',0,                            '8915',0
	db 'DoubleLongRightArrow',0,           '10233',0
	db 'DiacriticalDoubleAcute',0,         '733',0
	db 'Dashv',0,                          '10980',0
	db 'DScy',0,                           '1029',0
	db 'DDotrahd',0,                       '10513',0
	db 'DD',0,                             '8517',0
	db 'DJcy',0,                           '1026',0
	db 'Dagger',0,                         '8225',0
	db 'DZcy',0,                           '1039',0
	db 'Darr',0,                           '8609',0
	db 'Delta',0,                          '916',0
	db 'Dcy',0,                            '1044',0
	db 'Dcaron',0,                         '270',0
	db 'Del',0,                            '8711',0
	db 'DiacriticalAcute',0,               '180',0
	db 'Dfr',0,                            '120071',0
	db 'DiacriticalDot',0,                 '729',0
	db 'DotEqual',0,                       '8784',0
	db 'DifferentialD',0,                  '8518',0
	db 'DiacriticalTilde',0,               '732',0
	db 'DiacriticalGrave',0,               '96',0
	db 'Diamond',0,                        '8900',0
	db 'Dot',0,                            '168',0
	db 'Dopf',0,                           '120123',0
	db 'DotDot',0,                         '8412',0
	db 'DoubleLeftArrow',0,                '8656',0
	db 'DoubleDot',0,                      '168',0
	db 'DoubleContourIntegral',0,          '8751',0
	db 'DoubleDownArrow',0,                '8659',0
	db 'DoubleLeftTee',0,                  '10980',0
	db 'DoubleLeftRightArrow',0,           '8660',0
	db 'DoubleLongLeftArrow',0,            '10232',0
	db 'DoubleLongLeftRightArrow',0,       '10234',0
	db 'DownRightVectorBar',0,             '10583',0
	db 'DownArrowUpArrow',0,               '8693',0
	db 'DoubleUpDownArrow',0,              '8661',0
	db 'DoubleRightTee',0,                 '8872',0
	db 'DoubleRightArrow',0,               '8658',0
	db 'DoubleUpArrow',0,                  '8657',0
	db 'DownArrow',0,                      '8595',0
	db 'DoubleVerticalBar',0,              '8741',0
	db 'DownArrowBar',0,                   '10515',0
	db 'DownLeftVector',0,                 '8637',0
	db 'DownLeftRightVector',0,            '10576',0
	db 'DownBreve',0,                      '785',0
	db 'DownLeftTeeVector',0,              '10590',0
	db 'DownRightTeeVector',0,             '10591',0
	db 'DownLeftVectorBar',0,              '10582',0
	db 'DownRightVector',0,                '8641',0
	db 'Eacute',0,                         '201',0
	db 'Dscr',0,                           '119967',0
	db 'DownTeeArrow',0,                   '8615',0
	db 'DownTee',0,                        '8868',0
	db 'Downarrow',0,                      '8659',0
	db 'ENG',0,                            '330',0
	db 'Dstrok',0,                         '272',0
	db 'ETH',0,                            '208',0
	db 'Edot',0,                           '278',0
	db 'Ecirc',0,                          '202',0
	db 'Ecaron',0,                         '282',0
	db 'Ecy',0,                            '1069',0
	db 'Egrave',0,                         '200',0
	db 'Efr',0,                            '120072',0
	db 'Element',0,                        '8712',0
	db 'Emacr',0,                          '274',0
	db 'ImaginaryI',0,                     '8520',0
	db 'Gopf',0,                           '120126',0
	db 'FilledSmallSquare',0,              '9724',0
	db 'Escr',0,                           '8496',0
	db 'Epsilon',0,                        '917',0
	db 'Eogon',0,                          '280',0
	db 'EmptyVerySmallSquare',0,           '9643',0
	db 'Eopf',0,                           '120124',0
	db 'EqualTilde',0,                     '8770',0
	db 'Equal',0,                          '10869',0
	db 'Equilibrium',0,                    '8652',0
	db 'Exists',0,                         '8707',0
	db 'Eta',0,                            '919',0
	db 'Esim',0,                           '10867',0
	db 'Euml',0,                           '203',0
	db 'Fcy',0,                            '1060',0
	db 'ExponentialE',0,                   '8519',0
	db 'Ffr',0,                            '120073',0
	db 'Gamma',0,                          '915',0
	db 'Fouriertrf',0,                     '8497',0
	db 'Fopf',0,                           '120125',0
	db 'FilledVerySmallSquare',0,          '9642',0
	db 'ForAll',0,                         '8704',0
	db 'GJcy',0,                           '1027',0
	db 'Fscr',0,                           '8497',0
	db 'GT',0,                             '62',0
	db 'Gcirc',0,                          '284',0
	db 'Gbreve',0,                         '286',0
	db 'Gammad',0,                         '988',0
	db 'Gcedil',0,                         '290',0
	db 'Gdot',0,                           '288',0
	db 'Gcy',0,                            '1043',0
	db 'Gfr',0,                            '120074',0
	db 'Gg',0,                             '8921',0
	db 'Hopf',0,                           '8461',0
	db 'Gscr',0,                           '119970',0
	db 'GreaterGreater',0,                 '10914',0
	db 'GreaterEqualLess',0,               '8923',0
	db 'GreaterEqual',0,                   '8805',0
	db 'GreaterFullEqual',0,               '8807',0
	db 'GreaterSlantEqual',0,              '10878',0
	db 'GreaterLess',0,                    '8823',0
	db 'GreaterTilde',0,                   '8819',0
	db 'Hat',0,                            '94',0
	db 'HARDcy',0,                         '1066',0
	db 'Gt',0,                             '8811',0
	db 'Hacek',0,                          '711',0
	db 'Hfr',0,                            '8460',0
	db 'Hcirc',0,                          '292',0
	db 'HilbertSpace',0,                   '8459',0
	db 'IOcy',0,                           '1025',0
	db 'HumpDownHump',0,                   '8782',0
	db 'Hscr',0,                           '8459',0
	db 'HorizontalLine',0,                 '9472',0
	db 'Hstrok',0,                         '294',0
	db 'IEcy',0,                           '1045',0
	db 'HumpEqual',0,                      '8783',0
	db 'IJlig',0,                          '306',0
	db 'Idot',0,                           '304',0
	db 'Icirc',0,                          '206',0
	db 'Iacute',0,                         '205',0
	db 'Icy',0,                            '1048',0
	db 'Igrave',0,                         '204',0
	db 'Ifr',0,                            '8465',0
	db 'Im',0,                             '8465',0
	db 'Imacr',0,                          '298',0
	db 'Lang',0,                           '10218',0
	db 'Jfr',0,                            '120077',0
	db 'Iopf',0,                           '120128',0
	db 'Intersection',0,                   '8898',0
	db 'Int',0,                            '8748',0
	db 'Implies',0,                        '8658',0
	db 'Integral',0,                       '8747',0
	db 'InvisibleTimes',0,                 '8290',0
	db 'InvisibleComma',0,                 '8291',0
	db 'Iogon',0,                          '302',0
	db 'Iukcy',0,                          '1030',0
	db 'Iscr',0,                           '8464',0
	db 'Iota',0,                           '921',0
	db 'Itilde',0,                         '296',0
	db 'Jcirc',0,                          '308',0
	db 'Iuml',0,                           '207',0
	db 'Jcy',0,                            '1049',0
	db 'Kcedil',0,                         '310',0
	db 'Jukcy',0,                          '1028',0
	db 'Jscr',0,                           '119973',0
	db 'Jopf',0,                           '120129',0
	db 'Jsercy',0,                         '1032',0
	db 'KJcy',0,                           '1036',0
	db 'KHcy',0,                           '1061',0
	db 'Kappa',0,                          '922',0
	db 'Kscr',0,                           '119974',0
	db 'Kfr',0,                            '120078',0
	db 'Kcy',0,                            '1050',0
	db 'Kopf',0,                           '120130',0
	db 'LT',0,                             '60',0
	db 'LJcy',0,                           '1033',0
	db 'Lacute',0,                         '313',0
	db 'Lambda',0,                         '923',0
	db 'LeftRightArrow',0,                 '8596',0
	db 'LeftArrowBar',0,                   '8676',0
	db 'Lcedil',0,                         '315',0
	db 'Larr',0,                           '8606',0
	db 'Laplacetrf',0,                     '8466',0
	db 'Lcaron',0,                         '317',0
	db 'LeftAngleBracket',0,               '12296',0
	db 'Lcy',0,                            '1051',0
	db 'LeftArrow',0,                      '8592',0
	db 'LeftDownTeeVector',0,              '10593',0
	db 'LeftCeiling',0,                    '8968',0
	db 'LeftArrowRightArrow',0,            '8646',0
	db 'LeftDoubleBracket',0,              '10214',0
	db 'LeftDownVectorBar',0,              '10585',0
	db 'LeftDownVector',0,                 '8643',0
	db 'LeftFloor',0,                      '8970',0
	db 'LeftUpDownVector',0,               '10577',0
	db 'LeftTeeVector',0,                  '10586',0
	db 'LeftTee',0,                        '8867',0
	db 'LeftRightVector',0,                '10574',0
	db 'LeftTeeArrow',0,                   '8612',0
	db 'LeftTriangleBar',0,                '10703',0
	db 'LeftTriangle',0,                   '8882',0
	db 'LeftTriangleEqual',0,              '8884',0
	db 'LeftVector',0,                     '8636',0
	db 'LeftUpVector',0,                   '8639',0
	db 'LeftUpTeeVector',0,                '10592',0
	db 'LeftUpVectorBar',0,                '10584',0
	db 'Leftarrow',0,                      '8656',0
	db 'LeftVectorBar',0,                  '10578',0
	db 'Leftrightarrow',0,                 '8660',0
	db 'LessEqualGreater',0,               '8922',0
	db 'Prime',0,                          '8243',0
	db 'NotLessTilde',0,                   '8820',0
	db 'Ncedil',0,                         '325',0
	db 'LowerLeftArrow',0,                 '8601',0
	db 'Lmidot',0,                         '319',0
	db 'LessTilde',0,                      '8818',0
	db 'LessLess',0,                       '10913',0
	db 'LessGreater',0,                    '8822',0
	db 'LessSlantEqual',0,                 '10877',0
	db 'Ll',0,                             '8920',0
	db 'Lfr',0,                            '120079',0
	db 'Lleftarrow',0,                     '8666',0
	db 'Longleftarrow',0,                  '10232',0
	db 'LongLeftRightArrow',0,             '10231',0
	db 'LongLeftArrow',0,                  '10229',0
	db 'LongRightArrow',0,                 '10230',0
	db 'Longrightarrow',0,                 '10233',0
	db 'Longleftrightarrow',0,             '10234',0
	db 'Lopf',0,                           '120131',0
	db 'Mellintrf',0,                      '8499',0
	db 'Lstrok',0,                         '321',0
	db 'Lscr',0,                           '8466',0
	db 'LowerRightArrow',0,                '8600',0
	db 'Lsh',0,                            '8624',0
	db 'Map',0,                            '10501',0
	db 'Lt',0,                             '8810',0
	db 'Mcy',0,                            '1052',0
	db 'Mscr',0,                           '8499',0
	db 'MinusPlus',0,                      '8723',0
	db 'Mfr',0,                            '120080',0
	db 'Mopf',0,                           '120132',0
	db 'NJcy',0,                           '1034',0
	db 'Mu',0,                             '924',0
	db 'Nacute',0,                         '323',0
	db 'Ncaron',0,                         '327',0
	db 'NotGreater',0,                     '8815',0
	db 'Not',0,                            '10988',0
	db 'NewLine',0,                        '9226',0
	db 'NestedGreaterGreater',0,           '8811',0
	db 'Ncy',0,                            '1053',0
	db 'NestedLessLess',0,                 '8810',0
	db 'NoBreak',0,                        '8288',0
	db 'Nfr',0,                            '120081',0
	db 'Nopf',0,                           '8469',0
	db 'NotElement',0,                     '8713',0
	db 'NotCupCap',0,                      '8813',0
	db 'NotCongruent',0,                   '8802',0
	db 'NotDoubleVerticalBar',0,           '8742',0
	db 'NotEqualTilde',0,                  '8770,824',0
	db 'NotEqual',0,                       '8800',0
	db 'NotExists',0,                      '8708',0
	db 'NotHumpEqual',0,                   '8783,824',0
	db 'NotGreaterLess',0,                 '8825',0
	db 'NotGreaterFullEqual',0,            '8807,824',0
	db 'NotGreaterEqual',0,                '8817',0
	db 'NotGreaterGreater',0,              '8811,824',0
	db 'NotGreaterTilde',0,                '8821',0
	db 'NotGreaterSlantEqual',0,           '10878,824',0
	db 'NotHumpDownHump',0,                '8782,824',0
	db 'NotLess',0,                        '8814',0
	db 'NotLeftTriangleBar',0,             '10703,824',0
	db 'NotLeftTriangle',0,                '8938',0
	db 'NotLeftTriangleEqual',0,           '8940',0
	db 'NotLessGreater',0,                 '8824',0
	db 'NotLessEqual',0,                   '8816',0
	db 'NotLessLess',0,                    '8810,824',0
	db 'NotLessSlantEqual',0,              '10877,824',0
	db 'Ocy',0,                            '1054',0
	db 'NotSucceeds',0,                    '8833',0
	db 'NotRightTriangleBar',0,            '10704,824',0
	db 'NotPrecedesEqual',0,               '10927,824',0
	db 'NotNestedLessLess',0,              '10913,824',0
	db 'NotNestedGreaterGreater',0,        '10914,824',0
	db 'NotPrecedes',0,                    '8832',0
	db 'NotReverseElement',0,              '8716',0
	db 'NotPrecedesSlantEqual',0,          '8928',0
	db 'NotRightTriangle',0,               '8939',0
	db 'NotSquareSuperset',0,              '8848,824',0
	db 'NotSquareSubset',0,                '8847,824',0
	db 'NotRightTriangleEqual',0,          '8941',0
	db 'NotSquareSubsetEqual',0,           '8930',0
	db 'NotSubset',0,                      '8834,8402',0
	db 'NotSquareSupersetEqual',0,         '8931',0
	db 'NotSubsetEqual',0,                 '8840',0
	db 'NotTildeFullEqual',0,              '8775',0
	db 'NotSuperset',0,                    '8835,8402',0
	db 'NotSucceedsSlantEqual',0,          '8929',0
	db 'NotSucceedsEqual',0,               '10928,824',0
	db 'NotSucceedsTilde',0,               '8831,824',0
	db 'NotTilde',0,                       '8769',0
	db 'NotSupersetEqual',0,               '8841',0
	db 'NotTildeEqual',0,                  '8772',0
	db 'Ntilde',0,                         '209',0
	db 'NotVerticalBar',0,                 '8740',0
	db 'NotTildeTilde',0,                  '8777',0
	db 'Nscr',0,                           '119977',0
	db 'OElig',0,                          '338',0
	db 'Nu',0,                             '925',0
	db 'Oacute',0,                         '211',0
	db 'Ocirc',0,                          '212',0
	db 'OverBar',0,                        '8254',0
	db 'OpenCurlyDoubleQuote',0,           '8220',0
	db 'Omacr',0,                          '332',0
	db 'Ofr',0,                            '120082',0
	db 'Odblac',0,                         '336',0
	db 'Ograve',0,                         '210',0
	db 'Omicron',0,                        '927',0
	db 'Omega',0,                          '937',0
	db 'Oopf',0,                           '120134',0
	db 'Oslash',0,                         '216',0
	db 'Or',0,                             '10836',0
	db 'OpenCurlyQuote',0,                 '8216',0
	db 'Oscr',0,                           '119978',0
	db 'Otimes',0,                         '10807',0
	db 'Otilde',0,                         '213',0
	db 'Ouml',0,                           '214',0
	db 'Pi',0,                             '928',0
	db 'PartialD',0,                       '8706',0
	db 'OverBracket',0,                    '9140',0
	db 'OverBrace',0,                      '9182',0
	db 'OverParenthesis',0,                '9180',0
	db 'Pfr',0,                            '120083',0
	db 'Pcy',0,                            '1055',0
	db 'Phi',0,                            '934',0
	db 'Pr',0,                             '10939',0
	db 'Poincareplane',0,                  '8460',0
	db 'PlusMinus',0,                      '177',0
	db 'Popf',0,                           '8473',0
	db 'PrecedesEqual',0,                  '10927',0
	db 'Precedes',0,                       '8826',0
	db 'PrecedesSlantEqual',0,             '8828',0
	db 'PrecedesTilde',0,                  '8830',0
	db 'ShortRightArrow',0,                '8594',0
	db 'RightDownVectorBar',0,             '10581',0
	db 'Rcaron',0,                         '344',0
	db 'Qopf',0,                           '8474',0
	db 'Pscr',0,                           '119979',0
	db 'Proportion',0,                     '8759',0
	db 'Product',0,                        '8719',0
	db 'Proportional',0,                   '8733',0
	db 'QUOT',0,                           '34',0
	db 'Psi',0,                            '936',0
	db 'Qfr',0,                            '120084',0
	db 'Racute',0,                         '340',0
	db 'RBarr',0,                          '10512',0
	db 'Qscr',0,                           '119980',0
	db 'REG',0,                            '174',0
	db 'Rarr',0,                           '8608',0
	db 'Rang',0,                           '10219',0
	db 'Rarrtl',0,                         '10518',0
	db 'Rho',0,                            '929',0
	db 'ReverseElement',0,                 '8715',0
	db 'Rcy',0,                            '1056',0
	db 'Rcedil',0,                         '342',0
	db 'Re',0,                             '8476',0
	db 'ReverseUpEquilibrium',0,           '10607',0
	db 'ReverseEquilibrium',0,             '8651',0
	db 'Rfr',0,                            '8476',0
	db 'RightArrowLeftArrow',0,            '8644',0
	db 'RightArrow',0,                     '8594',0
	db 'RightAngleBracket',0,              '12297',0
	db 'RightArrowBar',0,                  '8677',0
	db 'RightDoubleBracket',0,             '10215',0
	db 'RightCeiling',0,                   '8969',0
	db 'RightDownTeeVector',0,             '10589',0
	db 'RightDownVector',0,                '8642',0
	db 'RoundImplies',0,                   '10608',0
	db 'RightUpDownVector',0,              '10575',0
	db 'RightTeeVector',0,                 '10587',0
	db 'RightTee',0,                       '8866',0
	db 'RightFloor',0,                     '8971',0
	db 'RightTeeArrow',0,                  '8614',0
	db 'RightTriangleBar',0,               '10704',0
	db 'RightTriangle',0,                  '8883',0
	db 'RightTriangleEqual',0,             '8885',0
	db 'RightVector',0,                    '8640',0
	db 'RightUpVector',0,                  '8638',0
	db 'RightUpTeeVector',0,               '10588',0
	db 'RightUpVectorBar',0,               '10580',0
	db 'Rightarrow',0,                     '8658',0
	db 'RightVectorBar',0,                 '10579',0
	db 'Ropf',0,                           '8477',0
	db 'Sacute',0,                         '346',0
	db 'RuleDelayed',0,                    '10740',0
	db 'Rscr',0,                           '8475',0
	db 'Rrightarrow',0,                    '8667',0
	db 'Rsh',0,                            '8625',0
	db 'SHcy',0,                           '1064',0
	db 'SHCHcy',0,                         '1065',0
	db 'SOFTcy',0,                         '1068',0
	db 'Scirc',0,                          '348',0
	db 'Scaron',0,                         '352',0
	db 'Sc',0,                             '10940',0
	db 'Scedil',0,                         '350',0
	db 'Sfr',0,                            '120086',0
	db 'Scy',0,                            '1057',0
	db 'ShortDownArrow',0,                 '8595',0
	db 'ShortLeftArrow',0,                 '8592',0
	db 'Tau',0,                            '932',0
	db 'Subset',0,                         '8912',0
	db 'SquareSubset',0,                   '8847',0
	db 'Sopf',0,                           '120138',0
	db 'Sigma',0,                          '931',0
	db 'ShortUpArrow',0,                   '8593',0
	db 'SmallCircle',0,                    '8728',0
	db 'Square',0,                         '9633',0
	db 'Sqrt',0,                           '8730',0
	db 'SquareIntersection',0,             '8851',0
	db 'SquareUnion',0,                    '8852',0
	db 'SquareSuperset',0,                 '8848',0
	db 'SquareSubsetEqual',0,              '8849',0
	db 'SquareSupersetEqual',0,            '8850',0
	db 'Star',0,                           '8902',0
	db 'Sscr',0,                           '119982',0
	db 'Sub',0,                            '8912',0
	db 'Sup',0,                            '8913',0
	db 'SucceedsSlantEqual',0,             '8829',0
	db 'Succeeds',0,                       '8827',0
	db 'SubsetEqual',0,                    '8838',0
	db 'SucceedsEqual',0,                  '10928',0
	db 'SuchThat',0,                       '8715',0
	db 'SucceedsTilde',0,                  '8831',0
	db 'Sum',0,                            '8721',0
	db 'THORN',0,                          '222',0
	db 'SupersetEqual',0,                  '8839',0
	db 'Superset',0,                       '8835',0
	db 'Supset',0,                         '8913',0
	db 'TSHcy',0,                          '1035',0
	db 'TRADE',0,                          '8482',0
	db 'TScy',0,                           '1062',0
	db 'Tab',0,                            '9225',0
	db 'Uarr',0,                           '8607',0
	db 'TildeEqual',0,                     '8771',0
	db 'Tfr',0,                            '120087',0
	db 'Tcedil',0,                         '354',0
	db 'Tcaron',0,                         '356',0
	db 'Tcy',0,                            '1058',0
	db 'Theta',0,                          '920',0
	db 'Therefore',0,                      '8756',0
	db 'Tilde',0,                          '8764',0
	db 'TripleDot',0,                      '8411',0
	db 'TildeTilde',0,                     '8776',0
	db 'TildeFullEqual',0,                 '8773',0
	db 'Topf',0,                           '120139',0
	db 'Tstrok',0,                         '358',0
	db 'Tscr',0,                           '119983',0
	db 'Uacute',0,                         '218',0
	db 'Ugrave',0,                         '217',0
	db 'Ucirc',0,                          '219',0
	db 'Ubrcy',0,                          '1038',0
	db 'Uarrocir',0,                       '10569',0
	db 'Ubreve',0,                         '364',0
	db 'Udblac',0,                         '368',0
	db 'Ucy',0,                            '1059',0
	db 'Ufr',0,                            '120088',0
	db 'UnderBracket',0,                   '9141',0
	db 'UnderBar',0,                       '95',0
	db 'Umacr',0,                          '362',0
	db 'UnderBrace',0,                     '9183',0
	db 'Union',0,                          '8899',0
	db 'UnderParenthesis',0,               '9181',0
	db 'UnionPlus',0,                      '8846',0
	db 'Uogon',0,                          '370',0
	db 'cir',0,                            '9675',0
	db 'bbrk',0,                           '9141',0
	db 'acE',0,                            '8766,819',0
	db 'Vscr',0,                           '119985',0
	db 'Utilde',0,                         '360',0
	db 'Uparrow',0,                        '8657',0
	db 'UpDownArrow',0,                    '8597',0
	db 'UpArrowBar',0,                     '10514',0
	db 'UpArrow',0,                        '8593',0
	db 'UpArrowDownArrow',0,               '8645',0
	db 'UpTee',0,                          '8869',0
	db 'UpEquilibrium',0,                  '10606',0
	db 'UpTeeArrow',0,                     '8613',0
	db 'Upsi',0,                           '978',0
	db 'UpperLeftArrow',0,                 '8598',0
	db 'Updownarrow',0,                    '8661',0
	db 'UpperRightArrow',0,                '8599',0
	db 'Uring',0,                          '366',0
	db 'Upsilon',0,                        '933',0
	db 'Uscr',0,                           '119984',0
	db 'Verbar',0,                         '8214',0
	db 'Vcy',0,                            '1042',0
	db 'VDash',0,                          '8875',0
	db 'Uuml',0,                           '220',0
	db 'Vbar',0,                           '10987',0
	db 'Vdashl',0,                         '10982',0
	db 'Vdash',0,                          '8873',0
	db 'Vee',0,                            '8897',0
	db 'VerticalLine',0,                   '166',0
	db 'VerticalBar',0,                    '8739',0
	db 'Vert',0,                           '8214',0
	db 'VerticalLine',0,                   '124',0
	db 'VerticalTilde',0,                  '8768',0
	db 'VerticalSeparator',0,              '10072',0
	db 'Vfr',0,                            '120089',0
	db 'Vopf',0,                           '120141',0
	db 'Ycy',0,                            '1067',0
	db 'Xi',0,                             '926',0
	db 'Wfr',0,                            '120090',0
	db 'Wcirc',0,                          '372',0
	db 'Vvdash',0,                         '8874',0
	db 'Wedge',0,                          '8896',0
	db 'Wscr',0,                           '119986',0
	db 'Wopf',0,                           '120142',0
	db 'Xfr',0,                            '120091',0
	db 'YIcy',0,                           '1031',0
	db 'Xscr',0,                           '119987',0
	db 'Xopf',0,                           '120143',0
	db 'YAcy',0,                           '1071',0
	db 'Yacute',0,                         '221',0
	db 'YUcy',0,                           '1070',0
	db 'Ycirc',0,                          '374',0
	db 'Zcy',0,                            '1047',0
	db 'Yuml',0,                           '376',0
	db 'Yopf',0,                           '120144',0
	db 'Yfr',0,                            '120092',0
	db 'Yscr',0,                           '119988',0
	db 'Zacute',0,                         '377',0
	db 'ZHcy',0,                           '1046',0
	db 'Zcaron',0,                         '381',0
	db 'Zopf',0,                           '8484',0
	db 'Zeta',0,                           '918',0
	db 'Zdot',0,                           '379',0
	db 'Zfr',0,                            '8488',0
	db 'aacute',0,                         '225',0
	db 'Zscr',0,                           '119989',0
	db 'abreve',0,                         '259',0
	db 'ac',0,                             '8766',0
	db 'angrtvb',0,                        '8894',0
	db 'andand',0,                         '10837',0
	db 'agrave',0,                         '224',0
	db 'acy',0,                            '1072',0
	db 'acirc',0,                          '226',0
	db 'acd',0,                            '8767',0
	db 'acute',0,                          '180',0
	db 'af',0,                             '8289',0
	db 'aelig',0,                          '230',0
	db 'afr',0,                            '120094',0
	db 'amacr',0,                          '257',0
	db 'aleph',0,                          '8501',0
	db 'alefsym',0,                        '8501',0
	db 'alpha',0,                          '945',0
	db 'amp',0,                            '38',0
	db 'amalg',0,                          '10815',0
	db 'and',0,                            '8743',0
	db 'angmsdaa',0,                       '10664',0
	db 'ang',0,                            '8736',0
	db 'andslope',0,                       '10840',0
	db 'andd',0,                           '10844',0
	db 'andv',0,                           '10842',0
	db 'angle',0,                          '8736',0
	db 'ange',0,                           '10660',0
	db 'angmsd',0,                         '8737',0
	db 'angmsdae',0,                       '10668',0
	db 'angmsdac',0,                       '10666',0
	db 'angmsdab',0,                       '10665',0
	db 'angmsdad',0,                       '10667',0
	db 'angmsdag',0,                       '10670',0
	db 'angmsdaf',0,                       '10669',0
	db 'angmsdah',0,                       '10671',0
	db 'angrt',0,                          '8735',0
	db 'ascr',0,                           '119990',0
	db 'apE',0,                            '10864',0
	db 'angzarr',0,                        '9084',0
	db 'angsph',0,                         '8738',0
	db 'angrtvbd',0,                       '10653',0
	db 'angst',0,                          '197',0
	db 'aopf',0,                           '120146',0
	db 'aogon',0,                          '261',0
	db 'ap',0,                             '8776',0
	db 'apos',0,                           '39',0
	db 'ape',0,                            '8778',0
	db 'apacir',0,                         '10863',0
	db 'apid',0,                           '8779',0
	db 'approxeq',0,                       '8778',0
	db 'approx',0,                         '8776',0
	db 'aring',0,                          '229',0
	db 'bNot',0,                           '10989',0
	db 'atilde',0,                         '227',0
	db 'asymp',0,                          '8776',0
	db 'ast',0,                            '42',0
	db 'asympeq',0,                        '8781',0
	db 'awconint',0,                       '8755',0
	db 'auml',0,                           '228',0
	db 'awint',0,                          '10769',0
	db 'backsim',0,                        '8765',0
	db 'backepsilon',0,                    '1014',0
	db 'backcong',0,                       '8780',0
	db 'backprime',0,                      '8245',0
	db 'barvee',0,                         '8893',0
	db 'backsimeq',0,                      '8909',0
	db 'barwed',0,                         '8965',0
	db 'barwedge',0,                       '8965',0
	db 'boxbox',0,                         '10697',0
	db 'blacktriangleright',0,             '9656',0
	db 'bigcup',0,                         '8899',0
	db 'bepsi',0,                          '1014',0
	db 'bdquo',0,                          '8222',0
	db 'bcong',0,                          '8780',0
	db 'bbrktbrk',0,                       '9142',0
	db 'bcy',0,                            '1073',0
	db 'because',0,                        '8757',0
	db 'becaus',0,                         '8757',0
	db 'bemptyv',0,                        '10672',0
	db 'between',0,                        '8812',0
	db 'beta',0,                           '946',0
	db 'bernou',0,                         '8492',0
	db 'beth',0,                           '8502',0
	db 'bigcap',0,                         '8898',0
	db 'bfr',0,                            '120095',0
	db 'bigcirc',0,                        '9711',0
	db 'biguplus',0,                       '10756',0
	db 'bigsqcup',0,                       '10758',0
	db 'bigoplus',0,                       '10753',0
	db 'bigodot',0,                        '10752',0
	db 'bigotimes',0,                      '10754',0
	db 'bigtriangledown',0,                '9661',0
	db 'bigstar',0,                        '9733',0
	db 'bigtriangleup',0,                  '9651',0
	db 'blacklozenge',0,                   '10731',0
	db 'bigwedge',0,                       '8896',0
	db 'bigvee',0,                         '8897',0
	db 'bkarow',0,                         '10509',0
	db 'blacktriangle',0,                  '9652',0
	db 'blacksquare',0,                    '9642',0
	db 'blacktriangledown',0,              '9662',0
	db 'blacktriangleleft',0,              '9666',0
	db 'boxDr',0,                          '9555',0
	db 'bnot',0,                           '8976',0
	db 'blk34',0,                          '9619',0
	db 'blk12',0,                          '9618',0
	db 'blank',0,                          '9251',0
	db 'blk14',0,                          '9617',0
	db 'bne',0,                            '61,8421',0
	db 'block',0,                          '9608',0
	db 'bnequiv',0,                        '8801,8421',0
	db 'bowtie',0,                         '8904',0
	db 'bot',0,                            '8869',0
	db 'bopf',0,                           '120147',0
	db 'bottom',0,                         '8869',0
	db 'boxDR',0,                          '9556',0
	db 'boxDL',0,                          '9559',0
	db 'boxDl',0,                          '9558',0
	db 'boxUl',0,                          '9564',0
	db 'boxHd',0,                          '9572',0
	db 'boxHD',0,                          '9574',0
	db 'boxH',0,                           '9552',0
	db 'boxHU',0,                          '9577',0
	db 'boxUL',0,                          '9565',0
	db 'boxHu',0,                          '9575',0
	db 'boxUR',0,                          '9562',0
	db 'boxVL',0,                          '9571',0
	db 'boxV',0,                           '9553',0
	db 'boxUr',0,                          '9561',0
	db 'boxVH',0,                          '9580',0
	db 'boxVh',0,                          '9579',0
	db 'boxVR',0,                          '9568',0
	db 'boxVl',0,                          '9570',0
	db 'boxVr',0,                          '9567',0
	db 'bsolhsub',0,                       '10184',0
	db 'boxur',0,                          '9492',0
	db 'boxhd',0,                          '9516',0
	db 'boxdr',0,                          '9484',0
	db 'boxdR',0,                          '9554',0
	db 'boxdL',0,                          '9557',0
	db 'boxdl',0,                          '9488',0
	db 'boxhD',0,                          '9573',0
	db 'boxh',0,                           '9472',0
	db 'boxhU',0,                          '9576',0
	db 'boxtimes',0,                       '8864',0
	db 'boxminus',0,                       '8863',0
	db 'boxhu',0,                          '9524',0
	db 'boxplus',0,                        '8862',0
	db 'boxuR',0,                          '9560',0
	db 'boxuL',0,                          '9563',0
	db 'boxul',0,                          '9496',0
	db 'bprime',0,                         '8245',0
	db 'boxvR',0,                          '9566',0
	db 'boxvH',0,                          '9578',0
	db 'boxv',0,                           '9474',0
	db 'boxvL',0,                          '9569',0
	db 'boxvl',0,                          '9508',0
	db 'boxvh',0,                          '9532',0
	db 'boxvr',0,                          '9500',0
	db 'bsemi',0,                          '8271',0
	db 'brvbar',0,                         '166',0
	db 'breve',0,                          '728',0
	db 'bscr',0,                           '119991',0
	db 'bsime',0,                          '8909',0
	db 'bsim',0,                           '8765',0
	db 'bsol',0,                           '92',0
	db 'bsolb',0,                          '10693',0
	db 'caron',0,                          '711',0
	db 'cap',0,                            '8745',0
	db 'bumpE',0,                          '10926',0
	db 'bullet',0,                         '8226',0
	db 'bull',0,                           '8226',0
	db 'bump',0,                           '8782',0
	db 'bumpeq',0,                         '8783',0
	db 'bumpe',0,                          '8783',0
	db 'cacute',0,                         '263',0
	db 'capcup',0,                         '10823',0
	db 'capbrcup',0,                       '10825',0
	db 'capand',0,                         '10820',0
	db 'capcap',0,                         '10827',0
	db 'caps',0,                           '8745,65024',0
	db 'capdot',0,                         '10816',0
	db 'caret',0,                          '8257',0
	db 'cedil',0,                          '184',0
	db 'ccirc',0,                          '265',0
	db 'ccaron',0,                         '269',0
	db 'ccaps',0,                          '10829',0
	db 'ccedil',0,                         '231',0
	db 'ccupssm',0,                        '10832',0
	db 'ccups',0,                          '10828',0
	db 'cdot',0,                           '267',0
	db 'cfr',0,                            '120096',0
	db 'cent',0,                           '162',0
	db 'cemptyv',0,                        '10674',0
	db 'centerdot',0,                      '183',0
	db 'check',0,                          '10003',0
	db 'chcy',0,                           '1095',0
	db 'checkmark',0,                      '10003',0
	db 'chi',0,                            '967',0
	db 'eDDot',0,                          '10871',0
	db 'cwint',0,                          '8753',0
	db 'crarr',0,                          '8629',0
	db 'clubsuit',0,                       '9827',0
	db 'circledast',0,                     '8859',0
	db 'circlearrowleft',0,                '8634',0
	db 'circ',0,                           '710',0
	db 'cirE',0,                           '10691',0
	db 'circeq',0,                         '8791',0
	db 'circledR',0,                       '174',0
	db 'circlearrowright',0,               '8635',0
	db 'circledS',0,                       '9416',0
	db 'cirfnint',0,                       '10768',0
	db 'circleddash',0,                    '8861',0
	db 'circledcirc',0,                    '8858',0
	db 'cire',0,                           '8791',0
	db 'cirscir',0,                        '10690',0
	db 'cirmid',0,                         '10991',0
	db 'clubs',0,                          '9827',0
	db 'complement',0,                     '8705',0
	db 'comma',0,                          '44',0
	db 'colone',0,                         '8788',0
	db 'colon',0,                          '58',0
	db 'coloneq',0,                        '8788',0
	db 'comp',0,                           '8705',0
	db 'commat',0,                         '64',0
	db 'compfn',0,                         '8728',0
	db 'conint',0,                         '8750',0
	db 'cong',0,                           '8773',0
	db 'complexes',0,                      '8450',0
	db 'congdot',0,                        '10861',0
	db 'coprod',0,                         '8720',0
	db 'copf',0,                           '120148',0
	db 'copy',0,                           '169',0
	db 'copysr',0,                         '8471',0
	db 'cupcap',0,                         '10822',0
	db 'cudarrl',0,                        '10552',0
	db 'csube',0,                          '10961',0
	db 'cscr',0,                           '119992',0
	db 'cross',0,                          '10007',0
	db 'csub',0,                           '10959',0
	db 'csupe',0,                          '10962',0
	db 'csup',0,                           '10960',0
	db 'ctdot',0,                          '8943',0
	db 'cularr',0,                         '8630',0
	db 'cuepr',0,                          '8926',0
	db 'cudarrr',0,                        '10549',0
	db 'cuesc',0,                          '8927',0
	db 'cup',0,                            '8746',0
	db 'cularrp',0,                        '10557',0
	db 'cupbrcap',0,                       '10824',0
	db 'curlyeqsucc',0,                    '8927',0
	db 'cups',0,                           '8746,65024',0
	db 'cupdot',0,                         '8845',0
	db 'cupcup',0,                         '10826',0
	db 'cupor',0,                          '10821',0
	db 'curarrm',0,                        '10556',0
	db 'curarr',0,                         '8631',0
	db 'curlyeqprec',0,                    '8926',0
	db 'curvearrowleft',0,                 '8630',0
	db 'curlywedge',0,                     '8911',0
	db 'curlyvee',0,                       '8910',0
	db 'curren',0,                         '164',0
	db 'cuvee',0,                          '8910',0
	db 'curvearrowright',0,                '8631',0
	db 'cuwed',0,                          '8911',0
	db 'cwconint',0,                       '8754',0
	db 'divideontimes',0,                  '8903',0
	db 'ddotseq',0,                        '10871',0
	db 'dashv',0,                          '8867',0
	db 'dagger',0,                         '8224',0
	db 'dArr',0,                           '8659',0
	db 'cylcty',0,                         '9005',0
	db 'dHar',0,                           '10597',0
	db 'darr',0,                           '8595',0
	db 'daleth',0,                         '8504',0
	db 'dash',0,                           '8208',0
	db 'dcy',0,                            '1076',0
	db 'dblac',0,                          '733',0
	db 'dbkarow',0,                        '10511',0
	db 'dcaron',0,                         '271',0
	db 'ddagger',0,                        '8225',0
	db 'dd',0,                             '8518',0
	db 'ddarr',0,                          '8650',0
	db 'diam',0,                           '8900',0
	db 'dfisht',0,                         '10623',0
	db 'delta',0,                          '948',0
	db 'deg',0,                            '176',0
	db 'demptyv',0,                        '10673',0
	db 'dharl',0,                          '8643',0
	db 'dfr',0,                            '120097',0
	db 'dharr',0,                          '8642',0
	db 'die',0,                            '168',0
	db 'diamondsuit',0,                    '9830',0
	db 'diamond',0,                        '8900',0
	db 'diams',0,                          '9830',0
	db 'disin',0,                          '8946',0
	db 'digamma',0,                        '989',0
	db 'div',0,                            '247',0
	db 'divide',0,                         '247',0
	db 'downharpoonleft',0,                '8643',0
	db 'doteq',0,                          '8784',0
	db 'dlcrop',0,                         '8973',0
	db 'djcy',0,                           '1106',0
	db 'divonx',0,                         '8903',0
	db 'dlcorn',0,                         '8990',0
	db 'dopf',0,                           '120149',0
	db 'dollar',0,                         '36',0
	db 'dot',0,                            '729',0
	db 'dotsquare',0,                      '8865',0
	db 'dotminus',0,                       '8760',0
	db 'doteqdot',0,                       '8785',0
	db 'dotplus',0,                        '8724',0
	db 'downarrow',0,                      '8595',0
	db 'doublebarwedge',0,                 '8966',0
	db 'downdownarrows',0,                 '8650',0
	db 'dstrok',0,                         '273',0
	db 'drcrop',0,                         '8972',0
	db 'drbkarow',0,                       '10512',0
	db 'downharpoonright',0,               '8642',0
	db 'drcorn',0,                         '8991',0
	db 'dscy',0,                           '1109',0
	db 'dscr',0,                           '119993',0
	db 'dsol',0,                           '10742',0
	db 'duarr',0,                          '8693',0
	db 'dtri',0,                           '9663',0
	db 'dtdot',0,                          '8945',0
	db 'dtrif',0,                          '9662',0
	db 'dwangle',0,                        '10662',0
	db 'duhar',0,                          '10607',0
	db 'dzcy',0,                           '1119',0
	db 'dzigrarr',0,                       '10239',0
	db 'fjlig',0,                          '102,106',0
	db 'epsilon',0,                        '949',0
	db 'egsdot',0,                         '10904',0
	db 'ecy',0,                            '1101',0
	db 'ecaron',0,                         '283',0
	db 'eacute',0,                         '233',0
	db 'eDot',0,                           '8785',0
	db 'easter',0,                         '10862',0
	db 'ecirc',0,                          '234',0
	db 'ecir',0,                           '8790',0
	db 'ecolon',0,                         '8789',0
	db 'efr',0,                            '120098',0
	db 'ee',0,                             '8519',0
	db 'edot',0,                           '279',0
	db 'efDot',0,                          '8786',0
	db 'egrave',0,                         '232',0
	db 'eg',0,                             '10906',0
	db 'egs',0,                            '10902',0
	db 'emptyset',0,                       '8709',0
	db 'els',0,                            '10901',0
	db 'elinters',0,                       '9191',0
	db 'el',0,                             '10905',0
	db 'ell',0,                            '8467',0
	db 'emacr',0,                          '275',0
	db 'elsdot',0,                         '10903',0
	db 'empty',0,                          '8709',0
	db 'eopf',0,                           '120150',0
	db 'eng',0,                            '331',0
	db 'emptyv',0,                         '8709',0
	db 'eogon',0,                          '281',0
	db 'eparsl',0,                         '10723',0
	db 'epar',0,                           '8917',0
	db 'eplus',0,                          '10865',0
	db 'epsi',0,                           '949',0
	db 'esim',0,                           '8770',0
	db 'equest',0,                         '8799',0
	db 'eqsim',0,                          '8770',0
	db 'eqcirc',0,                         '8790',0
	db 'epsiv',0,                          '1013',0
	db 'eqcolon',0,                        '8789',0
	db 'eqslantless',0,                    '10901',0
	db 'eqslantgtr',0,                     '10902',0
	db 'equals',0,                         '61',0
	db 'erDot',0,                          '8787',0
	db 'equivDD',0,                        '10872',0
	db 'equiv',0,                          '8801',0
	db 'eqvparsl',0,                       '10725',0
	db 'escr',0,                           '8495',0
	db 'erarr',0,                          '10609',0
	db 'esdot',0,                          '8784',0
	db 'exponentiale',0,                   '8519',0
	db 'euro',0,                           '8364',0
	db 'eth',0,                            '240',0
	db 'eta',0,                            '951',0
	db 'euml',0,                           '235',0
	db 'exist',0,                          '8707',0
	db 'excl',0,                           '33',0
	db 'expectation',0,                    '8496',0
	db 'ffilig',0,                         '64259',0
	db 'fcy',0,                            '1092',0
	db 'fallingdotseq',0,                  '8786',0
	db 'female',0,                         '9792',0
	db 'ffllig',0,                         '64260',0
	db 'fflig',0,                          '64256',0
	db 'ffr',0,                            '120099',0
	db 'filig',0,                          '64257',0
	db 'gap',0,                            '10886',0
	db 'frac23',0,                         '8532',0
	db 'forkv',0,                          '10969',0
	db 'fnof',0,                           '402',0
	db 'fllig',0,                          '64258',0
	db 'flat',0,                           '9837',0
	db 'fltns',0,                          '9649',0
	db 'forall',0,                         '8704',0
	db 'fopf',0,                           '120151',0
	db 'fork',0,                           '8916',0
	db 'frac14',0,                         '188',0
	db 'frac12',0,                         '189',0
	db 'fpartint',0,                       '10765',0
	db 'frac13',0,                         '8531',0
	db 'frac16',0,                         '8537',0
	db 'frac15',0,                         '8533',0
	db 'frac18',0,                         '8539',0
	db 'frac78',0,                         '8542',0
	db 'frac38',0,                         '8540',0
	db 'frac34',0,                         '190',0
	db 'frac25',0,                         '8534',0
	db 'frac35',0,                         '8535',0
	db 'frac56',0,                         '8538',0
	db 'frac45',0,                         '8536',0
	db 'frac58',0,                         '8541',0
	db 'gE',0,                             '8807',0
	db 'frown',0,                          '8994',0
	db 'frasl',0,                          '8260',0
	db 'fscr',0,                           '119995',0
	db 'gacute',0,                         '501',0
	db 'gEl',0,                            '10892',0
	db 'gamma',0,                          '947',0
	db 'gammad',0,                         '989',0
	db 'gesles',0,                         '10900',0
	db 'geqq',0,                           '8807',0
	db 'gdot',0,                           '289',0
	db 'gcirc',0,                          '285',0
	db 'gbreve',0,                         '287',0
	db 'gcy',0,                            '1075',0
	db 'gel',0,                            '8923',0
	db 'ge',0,                             '8805',0
	db 'geq',0,                            '8805',0
	db 'gesdot',0,                         '10880',0
	db 'ges',0,                            '10878',0
	db 'geqslant',0,                       '10878',0
	db 'gescc',0,                          '10921',0
	db 'gesdotol',0,                       '10884',0
	db 'gesdoto',0,                        '10882',0
	db 'gesl',0,                           '8923,65024',0
	db 'gla',0,                            '10917',0
	db 'gimel',0,                          '8503',0
	db 'gg',0,                             '8811',0
	db 'gfr',0,                            '120100',0
	db 'ggg',0,                            '8921',0
	db 'gl',0,                             '8823',0
	db 'gjcy',0,                           '1107',0
	db 'glE',0,                            '10898',0
	db 'gnapprox',0,                       '10890',0
	db 'gnE',0,                            '8809',0
	db 'glj',0,                            '10916',0
	db 'gnap',0,                           '10890',0
	db 'gneq',0,                           '10888',0
	db 'gne',0,                            '10888',0
	db 'gneqq',0,                          '8809',0
	db 'gnsim',0,                          '8935',0
	db 'part',0,                           '8706',0
	db 'lurdshar',0,                       '10570',0
	db 'laquo',0,                          '171',0
	db 'imagline',0,                       '8464',0
	db 'hellip',0,                         '8230',0
	db 'gtreqqless',0,                     '10892',0
	db 'gtcir',0,                          '10874',0
	db 'gsime',0,                          '10894',0
	db 'gscr',0,                           '8458',0
	db 'grave',0,                          '96',0
	db 'gsim',0,                           '8819',0
	db 'gt',0,                             '62',0
	db 'gsiml',0,                          '10896',0
	db 'gtcc',0,                           '10919',0
	db 'gtrapprox',0,                      '10886',0
	db 'gtlPar',0,                         '10645',0
	db 'gtdot',0,                          '8919',0
	db 'gtquest',0,                        '10876',0
	db 'gtrdot',0,                         '8919',0
	db 'gtrarr',0,                         '10616',0
	db 'gtreqless',0,                      '8923',0
	db 'hardcy',0,                         '1098',0
	db 'gvnE',0,                           '8809,65024',0
	db 'gtrsim',0,                         '8819',0
	db 'gtrless',0,                        '8823',0
	db 'gvertneqq',0,                      '8809,65024',0
	db 'half',0,                           '189',0
	db 'hArr',0,                           '8660',0
	db 'hamilt',0,                         '8459',0
	db 'hbar',0,                           '8463',0
	db 'harrcir',0,                        '10568',0
	db 'harr',0,                           '8596',0
	db 'harrw',0,                          '8621',0
	db 'hearts',0,                         '9829',0
	db 'hcirc',0,                          '293',0
	db 'heartsuit',0,                      '9829',0
	db 'iacute',0,                         '237',0
	db 'hookrightarrow',0,                 '8618',0
	db 'hkswarow',0,                       '10534',0
	db 'hfr',0,                            '120101',0
	db 'hercon',0,                         '8889',0
	db 'hksearow',0,                       '10533',0
	db 'homtht',0,                         '8763',0
	db 'hoarr',0,                          '8703',0
	db 'hookleftarrow',0,                  '8617',0
	db 'hslash',0,                         '8463',0
	db 'horbar',0,                         '8213',0
	db 'hopf',0,                           '120153',0
	db 'hscr',0,                           '119997',0
	db 'hybull',0,                         '8259',0
	db 'hstrok',0,                         '295',0
	db 'hyphen',0,                         '8208',0
	db 'igrave',0,                         '236',0
	db 'iecy',0,                           '1077',0
	db 'icirc',0,                          '238',0
	db 'ic',0,                             '8291',0
	db 'icy',0,                            '1080',0
	db 'iff',0,                            '8660',0
	db 'iexcl',0,                          '161',0
	db 'ifr',0,                            '120102',0
	db 'iinfin',0,                         '10716',0
	db 'iiiint',0,                         '10764',0
	db 'ii',0,                             '8520',0
	db 'iiint',0,                          '8749',0
	db 'ijlig',0,                          '307',0
	db 'iiota',0,                          '8489',0
	db 'imacr',0,                          '299',0
	db 'image',0,                          '8465',0
	db 'jcirc',0,                          '309',0
	db 'iocy',0,                           '1105',0
	db 'infintie',0,                       '10717',0
	db 'imped',0,                          '437',0
	db 'imath',0,                          '305',0
	db 'imagpart',0,                       '8465',0
	db 'imof',0,                           '8887',0
	db 'incare',0,                         '8453',0
	db 'in',0,                             '8712',0
	db 'infin',0,                          '8734',0
	db 'integers',0,                       '8484',0
	db 'int',0,                            '8747',0
	db 'inodot',0,                         '305',0
	db 'intcal',0,                         '8890',0
	db 'intlarhk',0,                       '10775',0
	db 'intercal',0,                       '8890',0
	db 'intprod',0,                        '10812',0
	db 'isinE',0,                          '8953',0
	db 'iprod',0,                          '10812',0
	db 'iopf',0,                           '120154',0
	db 'iogon',0,                          '303',0
	db 'iota',0,                           '953',0
	db 'iscr',0,                           '119998',0
	db 'iquest',0,                         '191',0
	db 'isin',0,                           '8712',0
	db 'isinv',0,                          '8712',0
	db 'isins',0,                          '8948',0
	db 'isindot',0,                        '8949',0
	db 'isinsv',0,                         '8947',0
	db 'itilde',0,                         '297',0
	db 'it',0,                             '8290',0
	db 'iukcy',0,                          '1110',0
	db 'iuml',0,                           '239',0
	db 'kopf',0,                           '120156',0
	db 'kappa',0,                          '954',0
	db 'jopf',0,                           '120155',0
	db 'jfr',0,                            '120103',0
	db 'jcy',0,                            '1081',0
	db 'jmath',0,                          '567',0
	db 'jsercy',0,                         '1112',0
	db 'jscr',0,                           '119999',0
	db 'jukcy',0,                          '1108',0
	db 'kfr',0,                            '120104',0
	db 'kcedil',0,                         '311',0
	db 'kappav',0,                         '1008',0
	db 'kcy',0,                            '1082',0
	db 'khcy',0,                           '1093',0
	db 'kgreen',0,                         '312',0
	db 'kjcy',0,                           '1116',0
	db 'lHar',0,                           '10594',0
	db 'lAtail',0,                         '10523',0
	db 'lAarr',0,                          '8666',0
	db 'kscr',0,                           '120000',0
	db 'lArr',0,                           '8656',0
	db 'lE',0,                             '8806',0
	db 'lBarr',0,                          '10510',0
	db 'lEg',0,                            '10891',0
	db 'lambda',0,                         '955',0
	db 'laemptyv',0,                       '10676',0
	db 'lacute',0,                         '314',0
	db 'lagran',0,                         '8466',0
	db 'langd',0,                          '10641',0
	db 'lang',0,                           '12296',0
	db 'langle',0,                         '12296',0
	db 'lap',0,                            '10885',0
	db 'lharu',0,                          '8636',0
	db 'leftarrow',0,                      '8592',0
	db 'lbrace',0,                         '123',0
	db 'larrsim',0,                        '10611',0
	db 'larrfs',0,                         '10525',0
	db 'larrb',0,                          '8676',0
	db 'larr',0,                           '8592',0
	db 'larrbfs',0,                        '10527',0
	db 'larrlp',0,                         '8619',0
	db 'larrhk',0,                         '8617',0
	db 'larrpl',0,                         '10553',0
	db 'late',0,                           '10925',0
	db 'lat',0,                            '10923',0
	db 'larrtl',0,                         '8610',0
	db 'latail',0,                         '10521',0
	db 'lbarr',0,                          '10508',0
	db 'lates',0,                          '10925,65024',0
	db 'lbbrk',0,                          '10098',0
	db 'lcub',0,                           '123',0
	db 'lbrkslu',0,                        '10637',0
	db 'lbrke',0,                          '10635',0
	db 'lbrack',0,                         '91',0
	db 'lbrksld',0,                        '10639',0
	db 'lcedil',0,                         '316',0
	db 'lcaron',0,                         '318',0
	db 'lceil',0,                          '8968',0
	db 'ldquor',0,                         '8222',0
	db 'ldca',0,                           '10550',0
	db 'lcy',0,                            '1083',0
	db 'ldquo',0,                          '8220',0
	db 'ldrushar',0,                       '10571',0
	db 'ldrdhar',0,                        '10599',0
	db 'ldsh',0,                           '8626',0
	db 'le',0,                             '8804',0
	db 'lesdot',0,                         '10879',0
	db 'leftrightsquigarrow',0,            '8621',0
	db 'leftleftarrows',0,                 '8647',0
	db 'leftharpoondown',0,                '8637',0
	db 'leftarrowtail',0,                  '8610',0
	db 'leftharpoonup',0,                  '8636',0
	db 'leftrightarrows',0,                '8646',0
	db 'leftrightarrow',0,                 '8596',0
	db 'leftrightharpoons',0,              '8651',0
	db 'leqq',0,                           '8806',0
	db 'leg',0,                            '8922',0
	db 'leftthreetimes',0,                 '8907',0
	db 'leq',0,                            '8804',0
	db 'les',0,                            '10877',0
	db 'leqslant',0,                       '10877',0
	db 'lescc',0,                          '10920',0
	db 'lesseqqgtr',0,                     '10891',0
	db 'lesges',0,                         '10899',0
	db 'lesdotor',0,                       '10883',0
	db 'lesdoto',0,                        '10881',0
	db 'lesg',0,                           '8922,65024',0
	db 'lessdot',0,                        '8918',0
	db 'lessapprox',0,                     '10885',0
	db 'lesseqgtr',0,                      '8922',0
	db 'lfloor',0,                         '8970',0
	db 'lesssim',0,                        '8818',0
	db 'lessgtr',0,                        '8822',0
	db 'lfisht',0,                         '10620',0
	db 'lg',0,                             '8822',0
	db 'lfr',0,                            '120105',0
	db 'lgE',0,                            '10897',0
	db 'lhard',0,                          '8637',0
	db 'lowbar',0,                         '95',0
	db 'lneq',0,                           '10887',0
	db 'lltri',0,                          '9722',0
	db 'll',0,                             '8810',0
	db 'lhblk',0,                          '9604',0
	db 'lharul',0,                         '10602',0
	db 'ljcy',0,                           '1113',0
	db 'llcorner',0,                       '8990',0
	db 'llarr',0,                          '8647',0
	db 'llhard',0,                         '10603',0
	db 'lnE',0,                            '8808',0
	db 'lmoust',0,                         '9136',0
	db 'lmidot',0,                         '320',0
	db 'lmoustache',0,                     '9136',0
	db 'lnapprox',0,                       '10889',0
	db 'lnap',0,                           '10889',0
	db 'lne',0,                            '10887',0
	db 'longmapsto',0,                     '10236',0
	db 'loarr',0,                          '8701',0
	db 'lnsim',0,                          '8934',0
	db 'lneqq',0,                          '8808',0
	db 'loang',0,                          '10220',0
	db 'longleftarrow',0,                  '10229',0
	db 'lobrk',0,                          '10214',0
	db 'longleftrightarrow',0,             '10231',0
	db 'lopar',0,                          '10629',0
	db 'looparrowleft',0,                  '8619',0
	db 'longrightarrow',0,                 '10230',0
	db 'looparrowright',0,                 '8620',0
	db 'loplus',0,                         '10797',0
	db 'lopf',0,                           '120157',0
	db 'lotimes',0,                        '10804',0
	db 'lowast',0,                         '8727',0
	db 'lsimg',0,                          '10895',0
	db 'lrhar',0,                          '8651',0
	db 'lpar',0,                           '40',0
	db 'lozenge',0,                        '9674',0
	db 'loz',0,                            '9674',0
	db 'lozf',0,                           '10731',0
	db 'lrarr',0,                          '8646',0
	db 'lparlt',0,                         '10643',0
	db 'lrcorner',0,                       '8991',0
	db 'lscr',0,                           '120001',0
	db 'lrtri',0,                          '8895',0
	db 'lrhard',0,                         '10605',0
	db 'lsaquo',0,                         '8249',0
	db 'lsim',0,                           '8818',0
	db 'lsh',0,                            '8624',0
	db 'lsime',0,                          '10893',0
	db 'ltdot',0,                          '8918',0
	db 'lstrok',0,                         '322',0
	db 'lsquo',0,                          '8216',0
	db 'lsqb',0,                           '91',0
	db 'lsquor',0,                         '8218',0
	db 'ltcc',0,                           '10918',0
	db 'lt',0,                             '60',0
	db 'ltcir',0,                          '10873',0
	db 'ltquest',0,                        '10875',0
	db 'ltimes',0,                         '8905',0
	db 'lthree',0,                         '8907',0
	db 'ltlarr',0,                         '10614',0
	db 'ltri',0,                           '9667',0
	db 'ltrPar',0,                         '10646',0
	db 'ltrie',0,                          '8884',0
	db 'ltrif',0,                          '9666',0
	db 'notnivc',0,                        '8957',0
	db 'nbumpe',0,                         '8783,824',0
	db 'mnplus',0,                         '8723',0
	db 'mcy',0,                            '1084',0
	db 'maltese',0,                        '10016',0
	db 'mDDot',0,                          '8762',0
	db 'lvertneqq',0,                      '8808,65024',0
	db 'luruhar',0,                        '10598',0
	db 'lvnE',0,                           '8808,65024',0
	db 'male',0,                           '9794',0
	db 'macr',0,                           '175',0
	db 'malt',0,                           '10016',0
	db 'mapstoleft',0,                     '8612',0
	db 'mapsto',0,                         '8614',0
	db 'map',0,                            '8614',0
	db 'mapstodown',0,                     '8615',0
	db 'marker',0,                         '9646',0
	db 'mapstoup',0,                       '8613',0
	db 'mcomma',0,                         '10793',0
	db 'midcir',0,                         '10992',0
	db 'mho',0,                            '8487',0
	db 'measuredangle',0,                  '8737',0
	db 'mdash',0,                          '8212',0
	db 'mfr',0,                            '120106',0
	db 'mid',0,                            '8739',0
	db 'micro',0,                          '181',0
	db 'midast',0,                         '42',0
	db 'minusb',0,                         '8863',0
	db 'minus',0,                          '45',0
	db 'middot',0,                         '183',0
	db 'minus',0,                          '8722',0
	db 'minusdu',0,                        '10794',0
	db 'minusd',0,                         '8760',0
	db 'mlcp',0,                           '10971',0
	db 'mldr',0,                           '8230',0
	db 'nLtv',0,                           '8810,824',0
	db 'mumap',0,                          '8888',0
	db 'mscr',0,                           '120002',0
	db 'mopf',0,                           '120158',0
	db 'models',0,                         '8871',0
	db 'mp',0,                             '8723',0
	db 'mu',0,                             '956',0
	db 'mstpos',0,                         '8766',0
	db 'multimap',0,                       '8888',0
	db 'nLeftarrow',0,                     '8653',0
	db 'nGt',0,                            '8811,8402',0
	db 'nGg',0,                            '8921,824',0
	db 'nGtv',0,                           '8811,824',0
	db 'nLl',0,                            '8920,824',0
	db 'nLeftrightarrow',0,                '8654',0
	db 'nLt',0,                            '8810,8402',0
	db 'napE',0,                           '10864,824',0
	db 'nabla',0,                          '8711',0
	db 'nVDash',0,                         '8879',0
	db 'nRightarrow',0,                    '8655',0
	db 'nVdash',0,                         '8878',0
	db 'nang',0,                           '8736,8402',0
	db 'nacute',0,                         '324',0
	db 'nap',0,                            '8777',0
	db 'natur',0,                          '9838',0
	db 'napos',0,                          '329',0
	db 'napid',0,                          '8779,824',0
	db 'napprox',0,                        '8777',0
	db 'naturals',0,                       '8469',0
	db 'natural',0,                        '9838',0
	db 'nbsp',0,                           '32',0
	db 'nbump',0,                          '8782,824',0
	db 'ni',0,                             '8715',0
	db 'nesear',0,                         '10536',0
	db 'ndash',0,                          '8211',0
	db 'ncong',0,                          '8775',0
	db 'ncaron',0,                         '328',0
	db 'ncap',0,                           '10819',0
	db 'ncedil',0,                         '326',0
	db 'ncup',0,                           '10818',0
	db 'ncongdot',0,                       '10861,824',0
	db 'ncy',0,                            '1085',0
	db 'nearr',0,                          '8599',0
	db 'neArr',0,                          '8663',0
	db 'ne',0,                             '8800',0
	db 'nearhk',0,                         '10532',0
	db 'nedot',0,                          '8784,824',0
	db 'nearrow',0,                        '8599',0
	db 'nequiv',0,                         '8802',0
	db 'ngeqq',0,                          '8807,824',0
	db 'nfr',0,                            '120107',0
	db 'nexist',0,                         '8708',0
	db 'nesim',0,                          '8770,824',0
	db 'nexists',0,                        '8708',0
	db 'nge',0,                            '8817',0
	db 'ngE',0,                            '8807,824',0
	db 'ngeq',0,                           '8817',0
	db 'ngt',0,                            '8815',0
	db 'nges',0,                           '10878,824',0
	db 'ngeqslant',0,                      '10878,824',0
	db 'ngsim',0,                          '8821',0
	db 'nhArr',0,                          '8654',0
	db 'ngtr',0,                           '8815',0
	db 'nharr',0,                          '8622',0
	db 'nhpar',0,                          '10994',0
	db 'nless',0,                          '8814',0
	db 'nldr',0,                           '8229',0
	db 'njcy',0,                           '1114',0
	db 'nisd',0,                           '8954',0
	db 'nis',0,                            '8956',0
	db 'niv',0,                            '8715',0
	db 'nlE',0,                            '8806,824',0
	db 'nlArr',0,                          '8653',0
	db 'nlarr',0,                          '8602',0
	db 'nleq',0,                           '8816',0
	db 'nleftarrow',0,                     '8602',0
	db 'nle',0,                            '8816',0
	db 'nleftrightarrow',0,                '8622',0
	db 'nleqslant',0,                      '10877,824',0
	db 'nleqq',0,                          '8806,824',0
	db 'nles',0,                           '10877,824',0
	db 'notin',0,                          '8713',0
	db 'nltrie',0,                         '8940',0
	db 'nlt',0,                            '8814',0
	db 'nlsim',0,                          '8820',0
	db 'nltri',0,                          '8938',0
	db 'nopf',0,                           '120159',0
	db 'nmid',0,                           '8740',0
	db 'not',0,                            '172',0
	db 'notinvb',0,                        '8951',0
	db 'notindot',0,                       '8949,824',0
	db 'notinE',0,                         '8953,824',0
	db 'notinva',0,                        '8713',0
	db 'notni',0,                          '8716',0
	db 'notinvc',0,                        '8950',0
	db 'notniva',0,                        '8716',0
	db 'notnivb',0,                        '8958',0
	db 'nvrArr',0,                         '10499',0
	db 'nsube',0,                          '8840',0
	db 'nrtri',0,                          '8939',0
	db 'npre',0,                           '10927,824',0
	db 'npart',0,                          '8706,824',0
	db 'nparallel',0,                      '8742',0
	db 'npar',0,                           '8742',0
	db 'nparsl',0,                         '11005,8421',0
	db 'npr',0,                            '8832',0
	db 'npolint',0,                        '10772',0
	db 'nprcue',0,                         '8928',0
	db 'nrarr',0,                          '8603',0
	db 'npreceq',0,                        '10927,824',0
	db 'nprec',0,                          '8832',0
	db 'nrArr',0,                          '8655',0
	db 'nrarrw',0,                         '8605,824',0
	db 'nrarrc',0,                         '10547,824',0
	db 'nrightarrow',0,                    '8603',0
	db 'nsim',0,                           '8769',0
	db 'nsce',0,                           '10928,824',0
	db 'nsc',0,                            '8833',0
	db 'nrtrie',0,                         '8941',0
	db 'nsccue',0,                         '8929',0
	db 'nshortmid',0,                      '8740',0
	db 'nscr',0,                           '120003',0
	db 'nshortparallel',0,                 '8742',0
	db 'nspar',0,                          '8742',0
	db 'nsimeq',0,                         '8772',0
	db 'nsime',0,                          '8772',0
	db 'nsmid',0,                          '8740',0
	db 'nsqsupe',0,                        '8931',0
	db 'nsqsube',0,                        '8930',0
	db 'nsub',0,                           '8836',0
	db 'nsubE',0,                          '10949,824',0
	db 'ntrianglelefteq',0,                '8940',0
	db 'nsupe',0,                          '8841',0
	db 'nsucc',0,                          '8833',0
	db 'nsubseteq',0,                      '8840',0
	db 'nsubset',0,                        '8834,8402',0
	db 'nsubseteqq',0,                     '10949,824',0
	db 'nsup',0,                           '8837',0
	db 'nsucceq',0,                        '10928,824',0
	db 'nsupE',0,                          '10950,824',0
	db 'ntgl',0,                           '8825',0
	db 'nsupseteq',0,                      '8841',0
	db 'nsupset',0,                        '8835,8402',0
	db 'nsupseteqq',0,                     '10950,824',0
	db 'ntlg',0,                           '8824',0
	db 'ntilde',0,                         '241',0
	db 'ntriangleleft',0,                  '8938',0
	db 'nvap',0,                           '8781,8402',0
	db 'num',0,                            '35',0
	db 'ntrianglerighteq',0,               '8941',0
	db 'ntriangleright',0,                 '8939',0
	db 'nu',0,                             '957',0
	db 'nvDash',0,                         '8877',0
	db 'numero',0,                         '8470',0
	db 'nvHarr',0,                         '10500',0
	db 'nvinfin',0,                        '10718',0
	db 'nvge',0,                           '8805,8402',0
	db 'nvdash',0,                         '8876',0
	db 'nvgt',0,                           '62,8402',0
	db 'nvle',0,                           '8804,8402',0
	db 'nvlArr',0,                         '10498',0
	db 'nvlt',0,                           '60,8402',0
	db 'nvltrie',0,                        '8884,8402',0
	db 'omacr',0,                          '333',0
	db 'odiv',0,                           '10808',0
	db 'oS',0,                             '9416',0
	db 'nwarhk',0,                         '10531',0
	db 'nvsim',0,                          '8764,8402',0
	db 'nvrtrie',0,                        '8885,8402',0
	db 'nwArr',0,                          '8662',0
	db 'nwarrow',0,                        '8598',0
	db 'nwarr',0,                          '8598',0
	db 'nwnear',0,                         '10535',0
	db 'ocirc',0,                          '244',0
	db 'oast',0,                           '8859',0
	db 'oacute',0,                         '243',0
	db 'ocir',0,                           '8858',0
	db 'odash',0,                          '8861',0
	db 'ocy',0,                            '1086',0
	db 'odblac',0,                         '337',0
	db 'ogt',0,                            '10689',0
	db 'ofcir',0,                          '10687',0
	db 'odsold',0,                         '10684',0
	db 'odot',0,                           '8857',0
	db 'oelig',0,                          '339',0
	db 'ogon',0,                           '731',0
	db 'ofr',0,                            '120108',0
	db 'ograve',0,                         '242',0
	db 'olarr',0,                          '8634',0
	db 'ohm',0,                            '937',0
	db 'ohbar',0,                          '10677',0
	db 'oint',0,                           '8750',0
	db 'olcross',0,                        '10683',0
	db 'olcir',0,                          '10686',0
	db 'oline',0,                          '8254',0
	db 'olt',0,                            '10688',0
	db 'origof',0,                         '8886',0
	db 'oplus',0,                          '8853',0
	db 'ominus',0,                         '8854',0
	db 'omicron',0,                        '959',0
	db 'omega',0,                          '969',0
	db 'omid',0,                           '10678',0
	db 'opar',0,                           '10679',0
	db 'oopf',0,                           '120160',0
	db 'operp',0,                          '10681',0
	db 'order',0,                          '8500',0
	db 'orarr',0,                          '8635',0
	db 'or',0,                             '8744',0
	db 'ord',0,                            '10845',0
	db 'ordf',0,                           '170',0
	db 'orderof',0,                        '8500',0
	db 'ordm',0,                           '186',0
	db 'otimes',0,                         '8855',0
	db 'oscr',0,                           '8500',0
	db 'orslope',0,                        '10839',0
	db 'oror',0,                           '10838',0
	db 'orv',0,                            '10843',0
	db 'osol',0,                           '8856',0
	db 'oslash',0,                         '248',0
	db 'otilde',0,                         '245',0
	db 'par',0,                            '8741',0
	db 'ouml',0,                           '246',0
	db 'otimesas',0,                       '10806',0
	db 'ovbar',0,                          '9021',0
	db 'parallel',0,                       '8741',0
	db 'para',0,                           '182',0
	db 'parsim',0,                         '10995',0
	db 'parsl',0,                          '11005',0
	db 'strns',0,                          '175',0
	db 'rightharpoondown',0,               '8641',0
	db 'quatint',0,                        '10774',0
	db 'prE',0,                            '10931',0
	db 'planckh',0,                        '8462',0
	db 'phi',0,                            '966',0
	db 'permil',0,                         '8240',0
	db 'percnt',0,                         '37',0
	db 'pcy',0,                            '1087',0
	db 'period',0,                         '46',0
	db 'pertenk',0,                        '8241',0
	db 'perp',0,                           '8869',0
	db 'pfr',0,                            '120109',0
	db 'pi',0,                             '960',0
	db 'phmmat',0,                         '8499',0
	db 'phiv',0,                           '966',0
	db 'phone',0,                          '9742',0
	db 'piv',0,                            '982',0
	db 'pitchfork',0,                      '8916',0
	db 'planck',0,                         '8463',0
	db 'pluse',0,                          '10866',0
	db 'plusb',0,                          '8862',0
	db 'plus',0,                           '43',0
	db 'plankv',0,                         '8463',0
	db 'plusacir',0,                       '10787',0
	db 'plusdo',0,                         '8724',0
	db 'pluscir',0,                        '10786',0
	db 'plusdu',0,                         '10789',0
	db 'pm',0,                             '177',0
	db 'plussim',0,                        '10790',0
	db 'plusmn',0,                         '177',0
	db 'plustwo',0,                        '10791',0
	db 'popf',0,                           '120161',0
	db 'pointint',0,                       '10773',0
	db 'pound',0,                          '163',0
	db 'pr',0,                             '8826',0
	db 'prnsim',0,                         '8936',0
	db 'precnapprox',0,                    '10937',0
	db 'prec',0,                           '8826',0
	db 'prcue',0,                          '8828',0
	db 'prap',0,                           '10935',0
	db 'pre',0,                            '10927',0
	db 'preccurlyeq',0,                    '8828',0
	db 'precapprox',0,                     '10935',0
	db 'preceq',0,                         '10927',0
	db 'prime',0,                          '8242',0
	db 'precnsim',0,                       '8936',0
	db 'precneqq',0,                       '10933',0
	db 'precsim',0,                        '8830',0
	db 'prnE',0,                           '10933',0
	db 'primes',0,                         '8473',0
	db 'prnap',0,                          '10937',0
	db 'prurel',0,                         '8880',0
	db 'profsurf',0,                       '8979',0
	db 'profalar',0,                       '9006',0
	db 'prod',0,                           '8719',0
	db 'profline',0,                       '8978',0
	db 'propto',0,                         '8733',0
	db 'prop',0,                           '8733',0
	db 'prsim',0,                          '8830',0
	db 'qint',0,                           '10764',0
	db 'psi',0,                            '968',0
	db 'pscr',0,                           '120005',0
	db 'qfr',0,                            '120110',0
	db 'qprime',0,                         '8279',0
	db 'qopf',0,                           '120162',0
	db 'qscr',0,                           '120006',0
	db 'quaternions',0,                    '8461',0
	db 'rbarr',0,                          '10509',0
	db 'rangle',0,                         '12297',0
	db 'rHar',0,                           '10596',0
	db 'rAarr',0,                          '8667',0
	db 'questeq',0,                        '8799',0
	db 'quest',0,                          '63',0
	db 'quot',0,                           '34',0
	db 'rAtail',0,                         '10524',0
	db 'rArr',0,                           '8658',0
	db 'rBarr',0,                          '10511',0
	db 'raemptyv',0,                       '10675',0
	db 'racute',0,                         '341',0
	db 'race',0,                           '8765,817',0
	db 'radic',0,                          '8730',0
	db 'rangd',0,                          '10642',0
	db 'rang',0,                           '12297',0
	db 'range',0,                          '10661',0
	db 'rarrhk',0,                         '8618',0
	db 'rarrb',0,                          '8677',0
	db 'rarr',0,                           '8594',0
	db 'raquo',0,                          '187',0
	db 'rarrap',0,                         '10613',0
	db 'rarrc',0,                          '10547',0
	db 'rarrbfs',0,                        '10528',0
	db 'rarrfs',0,                         '10526',0
	db 'rarrtl',0,                         '8611',0
	db 'rarrpl',0,                         '10565',0
	db 'rarrlp',0,                         '8620',0
	db 'rarrsim',0,                        '10612',0
	db 'ratail',0,                         '10522',0
	db 'rarrw',0,                          '8605',0
	db 'ratio',0,                          '8758',0
	db 'rationals',0,                      '8474',0
	db 'rdsh',0,                           '8627',0
	db 'rcedil',0,                         '343',0
	db 'rbrke',0,                          '10636',0
	db 'rbrace',0,                         '125',0
	db 'rbbrk',0,                          '10099',0
	db 'rbrack',0,                         '93',0
	db 'rbrkslu',0,                        '10640',0
	db 'rbrksld',0,                        '10638',0
	db 'rcaron',0,                         '345',0
	db 'rdca',0,                           '10551',0
	db 'rcub',0,                           '125',0
	db 'rceil',0,                          '8969',0
	db 'rcy',0,                            '1088',0
	db 'rdquo',0,                          '8221',0
	db 'rdldhar',0,                        '10601',0
	db 'rdquor',0,                         '8221',0
	db 'rfloor',0,                         '8971',0
	db 'reals',0,                          '8477',0
	db 'realine',0,                        '8475',0
	db 'real',0,                           '8476',0
	db 'realpart',0,                       '8476',0
	db 'reg',0,                            '174',0
	db 'rect',0,                           '9645',0
	db 'rfisht',0,                         '10621',0
	db 'rharul',0,                         '10604',0
	db 'rhard',0,                          '8641',0
	db 'rfr',0,                            '120111',0
	db 'rharu',0,                          '8640',0
	db 'rhov',0,                           '1009',0
	db 'rho',0,                            '961',0
	db 'rightarrow',0,                     '8594',0
	db 'rightarrowtail',0,                 '8611',0
	db 'setmn',0,                          '8726',0
	db 'rtri',0,                           '9657',0
	db 'robrk',0,                          '10215',0
	db 'risingdotseq',0,                   '8787',0
	db 'rightrightarrows',0,               '8649',0
	db 'rightleftarrows',0,                '8644',0
	db 'rightharpoonup',0,                 '8640',0
	db 'rightleftharpoons',0,              '8652',0
	db 'rightthreetimes',0,                '8908',0
	db 'rightsquigarrow',0,                '8605',0
	db 'ring',0,                           '730',0
	db 'rmoustache',0,                     '9137',0
	db 'rlhar',0,                          '8652',0
	db 'rlarr',0,                          '8644',0
	db 'rmoust',0,                         '9137',0
	db 'roang',0,                          '10221',0
	db 'rnmid',0,                          '10990',0
	db 'roarr',0,                          '8702',0
	db 'rrarr',0,                          '8649',0
	db 'rotimes',0,                        '10805',0
	db 'ropf',0,                           '120163',0
	db 'ropar',0,                          '10630',0
	db 'roplus',0,                         '10798',0
	db 'rpargt',0,                         '10644',0
	db 'rpar',0,                           '41',0
	db 'rppolint',0,                       '10770',0
	db 'rsqb',0,                           '93',0
	db 'rscr',0,                           '120007',0
	db 'rsaquo',0,                         '8250',0
	db 'rsh',0,                            '8625',0
	db 'rsquor',0,                         '8217',0
	db 'rsquo',0,                          '8217',0
	db 'rthree',0,                         '8908',0
	db 'rtimes',0,                         '8906',0
	db 'scnE',0,                           '10934',0
	db 'sc',0,                             '8827',0
	db 'ruluhar',0,                        '10600',0
	db 'rtrif',0,                          '9656',0
	db 'rtrie',0,                          '8885',0
	db 'rtriltri',0,                       '10702',0
	db 'sacute',0,                         '347',0
	db 'rx',0,                             '8478',0
	db 'sbquo',0,                          '8218',0
	db 'sccue',0,                          '8829',0
	db 'scap',0,                           '10936',0
	db 'scE',0,                            '10932',0
	db 'scaron',0,                         '353',0
	db 'scedil',0,                         '351',0
	db 'sce',0,                            '10928',0
	db 'scirc',0,                          '349',0
	db 'sdote',0,                          '10854',0
	db 'scsim',0,                          '8831',0
	db 'scnsim',0,                         '8937',0
	db 'scnap',0,                          '10938',0
	db 'scpolint',0,                       '10771',0
	db 'sdot',0,                           '8901',0
	db 'scy',0,                            '1089',0
	db 'sdotb',0,                          '8865',0
	db 'searrow',0,                        '8600',0
	db 'searhk',0,                         '10533',0
	db 'seArr',0,                          '8664',0
	db 'searr',0,                          '8600',0
	db 'semi',0,                           '59',0
	db 'sect',0,                           '167',0
	db 'seswar',0,                         '10537',0
	db 'setminus',0,                       '8726',0
	db 'softcy',0,                         '1100',0
	db 'simeq',0,                          '8771',0
	db 'shortparallel',0,                  '8741',0
	db 'sharp',0,                          '9839',0
	db 'sfr',0,                            '120112',0
	db 'sext',0,                           '10038',0
	db 'sfrown',0,                         '8994',0
	db 'shcy',0,                           '1096',0
	db 'shchcy',0,                         '1097',0
	db 'shortmid',0,                       '8739',0
	db 'sigmav',0,                         '962',0
	db 'sigma',0,                          '963',0
	db 'shy',0,                            '173',0
	db 'sigmaf',0,                         '962',0
	db 'simdot',0,                         '10858',0
	db 'sim',0,                            '8764',0
	db 'sime',0,                           '8771',0
	db 'slarr',0,                          '8592',0
	db 'simlE',0,                          '10911',0
	db 'simgE',0,                          '10912',0
	db 'simg',0,                           '10910',0
	db 'siml',0,                           '10909',0
	db 'simplus',0,                        '10788',0
	db 'simne',0,                          '8774',0
	db 'simrarr',0,                        '10610',0
	db 'smid',0,                           '8739',0
	db 'smashp',0,                         '10803',0
	db 'smallsetminus',0,                  '8726',0
	db 'smeparsl',0,                       '10724',0
	db 'smt',0,                            '10922',0
	db 'smile',0,                          '8995',0
	db 'smte',0,                           '10924',0
	db 'smtes',0,                          '10924,65024',0
	db 'sqsup',0,                          '8848',0
	db 'sqcap',0,                          '8851',0
	db 'sopf',0,                           '120164',0
	db 'solb',0,                           '10692',0
	db 'sol',0,                            '47',0
	db 'solbar',0,                         '9023',0
	db 'spadesuit',0,                      '9824',0
	db 'spades',0,                         '9824',0
	db 'spar',0,                           '8741',0
	db 'sqsub',0,                          '8847',0
	db 'sqcup',0,                          '8852',0
	db 'sqcaps',0,                         '8851,65024',0
	db 'sqcups',0,                         '8852,65024',0
	db 'sqsubset',0,                       '8847',0
	db 'sqsube',0,                         '8849',0
	db 'sqsubseteq',0,                     '8849',0
	db 'srarr',0,                          '8594',0
	db 'squ',0,                            '9633',0
	db 'sqsupset',0,                       '8848',0
	db 'sqsupe',0,                         '8850',0
	db 'sqsupseteq',0,                     '8850',0
	db 'squarf',0,                         '9642',0
	db 'square',0,                         '9633',0
	db 'squf',0,                           '9642',0
	db 'sstarf',0,                         '8902',0
	db 'ssetmn',0,                         '8726',0
	db 'sscr',0,                           '120008',0
	db 'ssmile',0,                         '8995',0
	db 'starf',0,                          '9733',0
	db 'star',0,                           '9734',0
	db 'straightepsilon',0,                '1013',0
	db 'straightphi',0,                    '981',0
	db 'ulcorner',0,                       '8988',0
	db 'telrec',0,                         '8981',0
	db 'supE',0,                           '10950',0
	db 'subsim',0,                         '10951',0
	db 'subne',0,                          '8842',0
	db 'sube',0,                           '8838',0
	db 'subE',0,                           '10949',0
	db 'sub',0,                            '8834',0
	db 'subdot',0,                         '10941',0
	db 'submult',0,                        '10945',0
	db 'subedot',0,                        '10947',0
	db 'subnE',0,                          '10955',0
	db 'subseteq',0,                       '8838',0
	db 'subrarr',0,                        '10617',0
	db 'subplus',0,                        '10943',0
	db 'subset',0,                         '8834',0
	db 'subsetneq',0,                      '8842',0
	db 'subseteqq',0,                      '10949',0
	db 'subsetneqq',0,                     '10955',0
	db 'succneqq',0,                       '10934',0
	db 'succapprox',0,                     '10936',0
	db 'subsup',0,                         '10963',0
	db 'subsub',0,                         '10965',0
	db 'succ',0,                           '8827',0
	db 'succeq',0,                         '10928',0
	db 'succcurlyeq',0,                    '8829',0
	db 'succnapprox',0,                    '10938',0
	db 'sung',0,                           '9834',0
	db 'succsim',0,                        '8831',0
	db 'succnsim',0,                       '8937',0
	db 'sum',0,                            '8721',0
	db 'sup1',0,                           '185',0
	db 'sup',0,                            '8835',0
	db 'sup2',0,                           '178',0
	db 'sup3',0,                           '179',0
	db 'supsetneqq',0,                     '10956',0
	db 'supmult',0,                        '10946',0
	db 'supedot',0,                        '10948',0
	db 'supdsub',0,                        '10968',0
	db 'supdot',0,                         '10942',0
	db 'supe',0,                           '8839',0
	db 'suphsub',0,                        '10967',0
	db 'suphsol',0,                        '10185',0
	db 'suplarr',0,                        '10619',0
	db 'supset',0,                         '8835',0
	db 'supne',0,                          '8843',0
	db 'supnE',0,                          '10956',0
	db 'supplus',0,                        '10944',0
	db 'supseteqq',0,                      '10950',0
	db 'supseteq',0,                       '8839',0
	db 'supsetneq',0,                      '8843',0
	db 'swnwar',0,                         '10538',0
	db 'swArr',0,                          '8665',0
	db 'supsub',0,                         '10964',0
	db 'supsim',0,                         '10952',0
	db 'supsup',0,                         '10966',0
	db 'swarr',0,                          '8601',0
	db 'swarhk',0,                         '10534',0
	db 'swarrow',0,                        '8601',0
	db 'tbrk',0,                           '9140',0
	db 'target',0,                         '8982',0
	db 'szlig',0,                          '223',0
	db 'tau',0,                            '964',0
	db 'tcedil',0,                         '355',0
	db 'tcaron',0,                         '357',0
	db 'tcy',0,                            '1090',0
	db 'tdot',0,                           '8411',0
	db 'trianglerighteq',0,                '8885',0
	db 'timesd',0,                         '10800',0
	db 'thicksim',0,                       '8764',0
	db 'theta',0,                          '952',0
	db 'there4',0,                         '8756',0
	db 'tfr',0,                            '120113',0
	db 'therefore',0,                      '8756',0
	db 'thetav',0,                         '977',0
	db 'thetasym',0,                       '977',0
	db 'thickapprox',0,                    '8776',0
	db 'tilde',0,                          '732',0
	db 'thksim',0,                         '8764',0
	db 'thkap',0,                          '8776',0
	db 'thorn',0,                          '254',0
	db 'timesb',0,                         '8864',0
	db 'times',0,                          '215',0
	db 'timesbar',0,                       '10801',0
	db 'tosa',0,                           '10537',0
	db 'topbot',0,                         '9014',0
	db 'toea',0,                           '10536',0
	db 'tint',0,                           '8749',0
	db 'top',0,                            '8868',0
	db 'topf',0,                           '120165',0
	db 'topcir',0,                         '10993',0
	db 'topfork',0,                        '10970',0
	db 'triangledown',0,                   '9663',0
	db 'trade',0,                          '8482',0
	db 'tprime',0,                         '8244',0
	db 'triangle',0,                       '9653',0
	db 'trianglelefteq',0,                 '8884',0
	db 'triangleleft',0,                   '9667',0
	db 'triangleq',0,                      '8796',0
	db 'triangleright',0,                  '9657',0
	db 'uHar',0,                           '10595',0
	db 'tscr',0,                           '120009',0
	db 'triplus',0,                        '10809',0
	db 'trie',0,                           '8796',0
	db 'tridot',0,                         '9708',0
	db 'triminus',0,                       '10810',0
	db 'tritime',0,                        '10811',0
	db 'trisb',0,                          '10701',0
	db 'trpezium',0,                       '9186',0
	db 'twixt',0,                          '8812',0
	db 'tshcy',0,                          '1115',0
	db 'tscy',0,                           '1094',0
	db 'tstrok',0,                         '359',0
	db 'twoheadrightarrow',0,              '8608',0
	db 'twoheadleftarrow',0,               '8606',0
	db 'uArr',0,                           '8657',0
	db 'udblac',0,                         '369',0
	db 'ubreve',0,                         '365',0
	db 'uarr',0,                           '8593',0
	db 'uacute',0,                         '250',0
	db 'ubrcy',0,                          '1118',0
	db 'ucy',0,                            '1091',0
	db 'ucirc',0,                          '251',0
	db 'udarr',0,                          '8645',0
	db 'ugrave',0,                         '249',0
	db 'ufisht',0,                         '10622',0
	db 'udhar',0,                          '10606',0
	db 'ufr',0,                            '120114',0
	db 'uharr',0,                          '8638',0
	db 'uharl',0,                          '8639',0
	db 'uhblk',0,                          '9600',0
	db 'ulcorn',0,                         '8988',0
	db 'vrtri',0,                          '8883',0
	db 'vangrt',0,                         '10652',0
	db 'urcorn',0,                         '8989',0
	db 'updownarrow',0,                    '8597',0
	db 'uml',0,                            '168',0
	db 'ultri',0,                          '9720',0
	db 'ulcrop',0,                         '8975',0
	db 'umacr',0,                          '363',0
	db 'uopf',0,                           '120166',0
	db 'uogon',0,                          '371',0
	db 'uparrow',0,                        '8593',0
	db 'upsi',0,                           '965',0
	db 'upharpoonright',0,                 '8638',0
	db 'upharpoonleft',0,                  '8639',0
	db 'uplus',0,                          '8846',0
	db 'upsilon',0,                        '965',0
	db 'upsih',0,                          '978',0
	db 'upuparrows',0,                     '8648',0
	db 'utri',0,                           '9653',0
	db 'urtri',0,                          '9721',0
	db 'urcrop',0,                         '8974',0
	db 'urcorner',0,                       '8989',0
	db 'uring',0,                          '367',0
	db 'utdot',0,                          '8944',0
	db 'uscr',0,                           '120010',0
	db 'utilde',0,                         '361',0
	db 'uwangle',0,                        '10663',0
	db 'uuarr',0,                          '8648',0
	db 'utrif',0,                          '9652',0
	db 'uuml',0,                           '252',0
	db 'vBar',0,                           '10984',0
	db 'vArr',0,                           '8661',0
	db 'vBarv',0,                          '10985',0
	db 'vDash',0,                          '8872',0
	db 'vartriangleright',0,               '8883',0
	db 'varrho',0,                         '1009',0
	db 'varphi',0,                         '966',0
	db 'varkappa',0,                       '1008',0
	db 'varepsilon',0,                     '1013',0
	db 'varnothing',0,                     '8709',0
	db 'varpropto',0,                      '8733',0
	db 'varpi',0,                          '982',0
	db 'varr',0,                           '8597',0
	db 'varsupsetneq',0,                   '8843,65024',0
	db 'varsubsetneq',0,                   '8842,65024',0
	db 'varsigma',0,                       '962',0
	db 'varsubsetneqq',0,                  '10955,65024',0
	db 'vartheta',0,                       '977',0
	db 'varsupsetneqq',0,                  '10956,65024',0
	db 'vartriangleleft',0,                '8882',0
	db 'verbar',0,                         '166',0
	db 'veebar',0,                         '8891',0
	db 'vdash',0,                          '8866',0
	db 'vcy',0,                            '1074',0
	db 'vee',0,                            '8744',0
	db 'vellip',0,                         '8942',0
	db 'veeeq',0,                          '8794',0
	db 'verbar',0,                         '124',0
	db 'vltri',0,                          '8882',0
	db 'vert',0,                           '166',0
	db 'vert',0,                           '124',0
	db 'vfr',0,                            '120115',0
	db 'vnsup',0,                          '8835,8402',0
	db 'vnsub',0,                          '8834,8402',0
	db 'vopf',0,                           '120167',0
	db 'vprop',0,                          '8733',0
	db 'xotime',0,                         '10754',0
	db 'wreath',0,                         '8768',0
	db 'wedbar',0,                         '10847',0
	db 'vsupnE',0,                         '10956,65024',0
	db 'vsubnE',0,                         '10955,65024',0
	db 'vscr',0,                           '120011',0
	db 'vsubne',0,                         '8842,65024',0
	db 'vzigzag',0,                        '10650',0
	db 'vsupne',0,                         '8843,65024',0
	db 'wcirc',0,                          '373',0
	db 'wfr',0,                            '120116',0
	db 'wedgeq',0,                         '8793',0
	db 'wedge',0,                          '8743',0
	db 'weierp',0,                         '8472',0
	db 'wp',0,                             '8472',0
	db 'wopf',0,                           '120168',0
	db 'wr',0,                             '8768',0
	db 'xharr',0,                          '10231',0
	db 'xcup',0,                           '8899',0
	db 'xcap',0,                           '8898',0
	db 'wscr',0,                           '120012',0
	db 'xcirc',0,                          '9711',0
	db 'xfr',0,                            '120117',0
	db 'xdtri',0,                          '9661',0
	db 'xhArr',0,                          '10234',0
	db 'xmap',0,                           '10236',0
	db 'xlArr',0,                          '10232',0
	db 'xi',0,                             '958',0
	db 'xlarr',0,                          '10229',0
	db 'xodot',0,                          '10752',0
	db 'xnis',0,                           '8955',0
	db 'xopf',0,                           '120169',0
	db 'xoplus',0,                         '10753',0
	db 'yopf',0,                           '120170',0
	db 'xwedge',0,                         '8896',0
	db 'xsqcup',0,                         '10758',0
	db 'xrarr',0,                          '10230',0
	db 'xrArr',0,                          '10233',0
	db 'xscr',0,                           '120013',0
	db 'xutri',0,                          '9651',0
	db 'xuplus',0,                         '10756',0
	db 'xvee',0,                           '8897',0
	db 'ycy',0,                            '1099',0
	db 'yacy',0,                           '1103',0
	db 'yacute',0,                         '253',0
	db 'ycirc',0,                          '375',0
	db 'yfr',0,                            '120118',0
	db 'yen',0,                            '165',0
	db 'yicy',0,                           '1111',0
	db 'zeetrf',0,                         '8488',0
	db 'zacute',0,                         '378',0
	db 'yucy',0,                           '1102',0
	db 'yscr',0,                           '120014',0
	db 'yuml',0,                           '255',0
	db 'zcy',0,                            '1079',0
	db 'zcaron',0,                         '382',0
	db 'zdot',0,                           '380',0
	db 'zigrarr',0,                        '8669',0
	db 'zfr',0,                            '120119',0
	db 'zeta',0,                           '950',0
	db 'zhcy',0,                           '1078',0
	db 'zscr',0,                           '120015',0
	db 'zopf',0,                           '120171',0
	db 'zwj',0,                            '8205',0
	db 'zwnj',0,                           '8204',0
	db 0
end;

{$ENDIF}

