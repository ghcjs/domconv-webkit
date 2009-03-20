{-# OPTIONS -fglasgow-exts -cpp #-}
{-
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
% @(#) $Docid: Jul. 12th 2001  10:08  Sigbjorn Finne $
% @(#) $Contactid: sof@galconn.com $
%

A grammar for OMG CORBA IDL

-}
module OmgParser ( parseIDL ) where

import LexM
import Lex
import IDLToken
import IDLSyn
import BasicTypes
import Literal
{-
BEGIN_GHC_ONLY
import GlaExts
END_GHC_ONLY
-}
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.18.2

data HappyAbsSyn 
	= HappyTerminal (IDLToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Defn])
	| HappyAbsSyn6 (Defn)
	| HappyAbsSyn10 ((Id, Inherit))
	| HappyAbsSyn13 (Inherit)
	| HappyAbsSyn15 (String)
	| HappyAbsSyn18 (Type)
	| HappyAbsSyn19 (Expr)
	| HappyAbsSyn27 (UnaryOp)
	| HappyAbsSyn29 (Literal)
	| HappyAbsSyn32 ((Type, [Id]))
	| HappyAbsSyn38 ([Id])
	| HappyAbsSyn39 (Id)
	| HappyAbsSyn50 ([Member])
	| HappyAbsSyn51 (Member)
	| HappyAbsSyn54 ([Switch])
	| HappyAbsSyn55 (Switch)
	| HappyAbsSyn56 ([CaseLabel])
	| HappyAbsSyn57 (CaseLabel)
	| HappyAbsSyn58 (SwitchArm)
	| HappyAbsSyn60 ([(Id,[Attribute],Maybe Expr)])
	| HappyAbsSyn64 ((Id, [Expr]))
	| HappyAbsSyn65 ([Expr])
	| HappyAbsSyn68 (Bool)
	| HappyAbsSyn76 ([Param])
	| HappyAbsSyn78 (Param)
	| HappyAbsSyn79 (Attribute)
	| HappyAbsSyn80 (Maybe Raises)
	| HappyAbsSyn81 (Maybe Context)
	| HappyAbsSyn82 ([String])
	| HappyAbsSyn87 (IntegerLit)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int# 
	-> (IDLToken)
	-> HappyState (IDLToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (IDLToken) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277 :: () => Int# -> ({-HappyReduction (LexM) = -}
	   Int# 
	-> (IDLToken)
	-> HappyState (IDLToken) (HappyStk HappyAbsSyn -> (LexM) HappyAbsSyn)
	-> [HappyState (IDLToken) (HappyStk HappyAbsSyn -> (LexM) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (LexM) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176 :: () => ({-HappyReduction (LexM) = -}
	   Int# 
	-> (IDLToken)
	-> HappyState (IDLToken) (HappyStk HappyAbsSyn -> (LexM) HappyAbsSyn)
	-> [HappyState (IDLToken) (HappyStk HappyAbsSyn -> (LexM) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (LexM) HappyAbsSyn)

action_0 (4#) = happyGoto action_3
action_0 (5#) = happyGoto action_2
action_0 x = happyTcHack x happyReduce_2

action_1 (5#) = happyGoto action_2
action_1 x = happyTcHack x happyFail

action_2 (91#) = happyShift action_15
action_2 (92#) = happyShift action_16
action_2 (101#) = happyShift action_17
action_2 (116#) = happyShift action_18
action_2 (125#) = happyShift action_19
action_2 (126#) = happyShift action_20
action_2 (130#) = happyShift action_21
action_2 (154#) = happyShift action_22
action_2 (158#) = happyShift action_23
action_2 (159#) = happyShift action_24
action_2 (160#) = happyShift action_25
action_2 (6#) = happyGoto action_4
action_2 (7#) = happyGoto action_5
action_2 (8#) = happyGoto action_6
action_2 (9#) = happyGoto action_7
action_2 (10#) = happyGoto action_8
action_2 (17#) = happyGoto action_9
action_2 (31#) = happyGoto action_10
action_2 (49#) = happyGoto action_11
action_2 (52#) = happyGoto action_12
action_2 (59#) = happyGoto action_13
action_2 (70#) = happyGoto action_14
action_2 x = happyTcHack x happyReduce_1

action_3 (162#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 x = happyTcHack x happyReduce_3

action_5 (90#) = happyShift action_87
action_5 x = happyTcHack x happyFail

action_6 (90#) = happyShift action_86
action_6 x = happyTcHack x happyFail

action_7 x = happyTcHack x happyReduce_13

action_8 (95#) = happyShift action_85
action_8 x = happyTcHack x happyFail

action_9 (90#) = happyShift action_84
action_9 x = happyTcHack x happyFail

action_10 (90#) = happyShift action_83
action_10 x = happyTcHack x happyFail

action_11 x = happyTcHack x happyReduce_68

action_12 x = happyTcHack x happyReduce_69

action_13 x = happyTcHack x happyReduce_70

action_14 (90#) = happyShift action_82
action_14 x = happyTcHack x happyFail

action_15 (141#) = happyShift action_27
action_15 (89#) = happyGoto action_81
action_15 x = happyTcHack x happyFail

action_16 (141#) = happyShift action_27
action_16 (89#) = happyGoto action_80
action_16 x = happyTcHack x happyFail

action_17 (98#) = happyShift action_53
action_17 (118#) = happyShift action_54
action_17 (119#) = happyShift action_55
action_17 (120#) = happyShift action_56
action_17 (121#) = happyShift action_57
action_17 (122#) = happyShift action_58
action_17 (123#) = happyShift action_59
action_17 (124#) = happyShift action_60
action_17 (141#) = happyShift action_61
action_17 (146#) = happyShift action_62
action_17 (147#) = happyShift action_63
action_17 (153#) = happyShift action_79
action_17 (15#) = happyGoto action_69
action_17 (18#) = happyGoto action_70
action_17 (41#) = happyGoto action_71
action_17 (42#) = happyGoto action_72
action_17 (43#) = happyGoto action_73
action_17 (44#) = happyGoto action_74
action_17 (45#) = happyGoto action_75
action_17 (62#) = happyGoto action_76
action_17 (63#) = happyGoto action_77
action_17 (86#) = happyGoto action_78
action_17 x = happyTcHack x happyFail

action_18 (98#) = happyShift action_53
action_18 (118#) = happyShift action_54
action_18 (119#) = happyShift action_55
action_18 (120#) = happyShift action_56
action_18 (121#) = happyShift action_57
action_18 (122#) = happyShift action_58
action_18 (123#) = happyShift action_59
action_18 (124#) = happyShift action_60
action_18 (125#) = happyShift action_19
action_18 (126#) = happyShift action_20
action_18 (130#) = happyShift action_21
action_18 (141#) = happyShift action_61
action_18 (146#) = happyShift action_62
action_18 (147#) = happyShift action_63
action_18 (148#) = happyShift action_64
action_18 (149#) = happyShift action_65
action_18 (150#) = happyShift action_66
action_18 (151#) = happyShift action_67
action_18 (153#) = happyShift action_68
action_18 (15#) = happyGoto action_31
action_18 (32#) = happyGoto action_32
action_18 (33#) = happyGoto action_33
action_18 (34#) = happyGoto action_34
action_18 (35#) = happyGoto action_35
action_18 (36#) = happyGoto action_36
action_18 (37#) = happyGoto action_37
action_18 (41#) = happyGoto action_38
action_18 (42#) = happyGoto action_39
action_18 (43#) = happyGoto action_40
action_18 (44#) = happyGoto action_41
action_18 (45#) = happyGoto action_42
action_18 (46#) = happyGoto action_43
action_18 (47#) = happyGoto action_44
action_18 (48#) = happyGoto action_45
action_18 (49#) = happyGoto action_46
action_18 (52#) = happyGoto action_47
action_18 (59#) = happyGoto action_48
action_18 (61#) = happyGoto action_49
action_18 (62#) = happyGoto action_50
action_18 (63#) = happyGoto action_51
action_18 (85#) = happyGoto action_52
action_18 x = happyTcHack x happyFail

action_19 (141#) = happyShift action_27
action_19 (89#) = happyGoto action_30
action_19 x = happyTcHack x happyFail

action_20 (141#) = happyShift action_27
action_20 (89#) = happyGoto action_29
action_20 x = happyTcHack x happyFail

action_21 (141#) = happyShift action_27
action_21 (89#) = happyGoto action_28
action_21 x = happyTcHack x happyFail

action_22 (141#) = happyShift action_27
action_22 (89#) = happyGoto action_26
action_22 x = happyTcHack x happyFail

action_23 x = happyTcHack x happyReduce_10

action_24 x = happyTcHack x happyReduce_11

action_25 x = happyTcHack x happyReduce_9

action_26 (95#) = happyShift action_108
action_26 x = happyTcHack x happyFail

action_27 x = happyTcHack x happyReduce_176

action_28 (95#) = happyShift action_107
action_28 x = happyTcHack x happyFail

action_29 (127#) = happyShift action_106
action_29 x = happyTcHack x happyFail

action_30 (95#) = happyShift action_105
action_30 x = happyTcHack x happyFail

action_31 (98#) = happyShift action_94
action_31 x = happyTcHack x happyReduce_76

action_32 x = happyTcHack x happyReduce_67

action_33 (141#) = happyShift action_27
action_33 (38#) = happyGoto action_102
action_33 (40#) = happyGoto action_103
action_33 (89#) = happyGoto action_104
action_33 x = happyTcHack x happyFail

action_34 x = happyTcHack x happyReduce_72

action_35 x = happyTcHack x happyReduce_74

action_36 x = happyTcHack x happyReduce_75

action_37 x = happyTcHack x happyReduce_73

action_38 x = happyTcHack x happyReduce_77

action_39 x = happyTcHack x happyReduce_78

action_40 x = happyTcHack x happyReduce_79

action_41 x = happyTcHack x happyReduce_80

action_42 x = happyTcHack x happyReduce_81

action_43 x = happyTcHack x happyReduce_82

action_44 x = happyTcHack x happyReduce_83

action_45 x = happyTcHack x happyReduce_84

action_46 x = happyTcHack x happyReduce_89

action_47 x = happyTcHack x happyReduce_90

action_48 x = happyTcHack x happyReduce_91

action_49 x = happyTcHack x happyReduce_85

action_50 x = happyTcHack x happyReduce_86

action_51 x = happyTcHack x happyReduce_87

action_52 x = happyTcHack x happyReduce_88

action_53 (141#) = happyShift action_101
action_53 x = happyTcHack x happyFail

action_54 x = happyTcHack x happyReduce_97

action_55 x = happyTcHack x happyReduce_98

action_56 (119#) = happyShift action_100
action_56 x = happyTcHack x happyFail

action_57 (119#) = happyShift action_99
action_57 x = happyTcHack x happyFail

action_58 x = happyTcHack x happyReduce_101

action_59 x = happyTcHack x happyReduce_102

action_60 x = happyTcHack x happyReduce_103

action_61 x = happyTcHack x happyReduce_27

action_62 (131#) = happyShift action_98
action_62 x = happyTcHack x happyReduce_131

action_63 (131#) = happyShift action_97
action_63 x = happyTcHack x happyReduce_133

action_64 (131#) = happyShift action_96
action_64 x = happyTcHack x happyFail

action_65 x = happyTcHack x happyReduce_106

action_66 x = happyTcHack x happyReduce_105

action_67 x = happyTcHack x happyReduce_104

action_68 (131#) = happyShift action_95
action_68 x = happyTcHack x happyFail

action_69 (98#) = happyShift action_94
action_69 x = happyTcHack x happyReduce_41

action_70 (141#) = happyShift action_27
action_70 (89#) = happyGoto action_93
action_70 x = happyTcHack x happyFail

action_71 x = happyTcHack x happyReduce_37

action_72 x = happyTcHack x happyReduce_33

action_73 x = happyTcHack x happyReduce_34

action_74 x = happyTcHack x happyReduce_35

action_75 x = happyTcHack x happyReduce_36

action_76 x = happyTcHack x happyReduce_38

action_77 x = happyTcHack x happyReduce_39

action_78 x = happyTcHack x happyReduce_40

action_79 x = happyTcHack x happyReduce_173

action_80 (95#) = happyReduce_24
action_80 (97#) = happyShift action_92
action_80 (13#) = happyGoto action_90
action_80 (14#) = happyGoto action_91
action_80 x = happyTcHack x happyReduce_14

action_81 (95#) = happyShift action_89
action_81 x = happyTcHack x happyFail

action_82 x = happyTcHack x happyReduce_6

action_83 x = happyTcHack x happyReduce_4

action_84 x = happyTcHack x happyReduce_5

action_85 (11#) = happyGoto action_88
action_85 x = happyTcHack x happyReduce_17

action_86 x = happyTcHack x happyReduce_7

action_87 x = happyTcHack x happyReduce_8

action_88 (96#) = happyShift action_154
action_88 (101#) = happyShift action_17
action_88 (116#) = happyShift action_18
action_88 (125#) = happyShift action_19
action_88 (126#) = happyShift action_20
action_88 (130#) = happyShift action_21
action_88 (142#) = happyReduce_140
action_88 (152#) = happyShift action_155
action_88 (154#) = happyShift action_22
action_88 (157#) = happyShift action_156
action_88 (12#) = happyGoto action_146
action_88 (17#) = happyGoto action_147
action_88 (31#) = happyGoto action_148
action_88 (49#) = happyGoto action_11
action_88 (52#) = happyGoto action_12
action_88 (59#) = happyGoto action_13
action_88 (67#) = happyGoto action_149
action_88 (68#) = happyGoto action_150
action_88 (70#) = happyGoto action_151
action_88 (73#) = happyGoto action_152
action_88 (74#) = happyGoto action_153
action_88 x = happyTcHack x happyReduce_149

action_89 (5#) = happyGoto action_145
action_89 x = happyTcHack x happyReduce_2

action_90 x = happyTcHack x happyReduce_16

action_91 x = happyTcHack x happyReduce_25

action_92 (98#) = happyShift action_53
action_92 (141#) = happyShift action_61
action_92 (15#) = happyGoto action_144
action_92 x = happyTcHack x happyFail

action_93 (102#) = happyShift action_143
action_93 x = happyTcHack x happyFail

action_94 (141#) = happyShift action_142
action_94 x = happyTcHack x happyFail

action_95 (98#) = happyShift action_53
action_95 (113#) = happyShift action_135
action_95 (139#) = happyShift action_136
action_95 (141#) = happyShift action_61
action_95 (143#) = happyShift action_137
action_95 (145#) = happyShift action_138
action_95 (15#) = happyGoto action_122
action_95 (19#) = happyGoto action_123
action_95 (20#) = happyGoto action_124
action_95 (21#) = happyGoto action_125
action_95 (22#) = happyGoto action_126
action_95 (23#) = happyGoto action_127
action_95 (24#) = happyGoto action_128
action_95 (25#) = happyGoto action_129
action_95 (26#) = happyGoto action_130
action_95 (27#) = happyGoto action_131
action_95 (28#) = happyGoto action_132
action_95 (29#) = happyGoto action_133
action_95 (30#) = happyGoto action_141
action_95 x = happyTcHack x happyFail

action_96 (98#) = happyShift action_53
action_96 (118#) = happyShift action_54
action_96 (119#) = happyShift action_55
action_96 (120#) = happyShift action_56
action_96 (121#) = happyShift action_57
action_96 (122#) = happyShift action_58
action_96 (123#) = happyShift action_59
action_96 (124#) = happyShift action_60
action_96 (141#) = happyShift action_61
action_96 (146#) = happyShift action_62
action_96 (147#) = happyShift action_63
action_96 (148#) = happyShift action_64
action_96 (149#) = happyShift action_65
action_96 (150#) = happyShift action_66
action_96 (151#) = happyShift action_67
action_96 (153#) = happyShift action_68
action_96 (15#) = happyGoto action_31
action_96 (34#) = happyGoto action_140
action_96 (35#) = happyGoto action_35
action_96 (36#) = happyGoto action_36
action_96 (41#) = happyGoto action_38
action_96 (42#) = happyGoto action_39
action_96 (43#) = happyGoto action_40
action_96 (44#) = happyGoto action_41
action_96 (45#) = happyGoto action_42
action_96 (46#) = happyGoto action_43
action_96 (47#) = happyGoto action_44
action_96 (48#) = happyGoto action_45
action_96 (61#) = happyGoto action_49
action_96 (62#) = happyGoto action_50
action_96 (63#) = happyGoto action_51
action_96 (85#) = happyGoto action_52
action_96 x = happyTcHack x happyFail

action_97 (98#) = happyShift action_53
action_97 (113#) = happyShift action_135
action_97 (139#) = happyShift action_136
action_97 (141#) = happyShift action_61
action_97 (143#) = happyShift action_137
action_97 (145#) = happyShift action_138
action_97 (15#) = happyGoto action_122
action_97 (19#) = happyGoto action_123
action_97 (20#) = happyGoto action_124
action_97 (21#) = happyGoto action_125
action_97 (22#) = happyGoto action_126
action_97 (23#) = happyGoto action_127
action_97 (24#) = happyGoto action_128
action_97 (25#) = happyGoto action_129
action_97 (26#) = happyGoto action_130
action_97 (27#) = happyGoto action_131
action_97 (28#) = happyGoto action_132
action_97 (29#) = happyGoto action_133
action_97 (30#) = happyGoto action_139
action_97 x = happyTcHack x happyFail

action_98 (98#) = happyShift action_53
action_98 (113#) = happyShift action_135
action_98 (139#) = happyShift action_136
action_98 (141#) = happyShift action_61
action_98 (143#) = happyShift action_137
action_98 (145#) = happyShift action_138
action_98 (15#) = happyGoto action_122
action_98 (19#) = happyGoto action_123
action_98 (20#) = happyGoto action_124
action_98 (21#) = happyGoto action_125
action_98 (22#) = happyGoto action_126
action_98 (23#) = happyGoto action_127
action_98 (24#) = happyGoto action_128
action_98 (25#) = happyGoto action_129
action_98 (26#) = happyGoto action_130
action_98 (27#) = happyGoto action_131
action_98 (28#) = happyGoto action_132
action_98 (29#) = happyGoto action_133
action_98 (30#) = happyGoto action_134
action_98 x = happyTcHack x happyFail

action_99 x = happyTcHack x happyReduce_99

action_100 x = happyTcHack x happyReduce_100

action_101 x = happyTcHack x happyReduce_28

action_102 (99#) = happyShift action_121
action_102 x = happyTcHack x happyReduce_71

action_103 x = happyTcHack x happyReduce_92

action_104 (135#) = happyShift action_120
action_104 (65#) = happyGoto action_118
action_104 (66#) = happyGoto action_119
action_104 x = happyTcHack x happyReduce_95

action_105 (98#) = happyShift action_53
action_105 (118#) = happyShift action_54
action_105 (119#) = happyShift action_55
action_105 (120#) = happyShift action_56
action_105 (121#) = happyShift action_57
action_105 (122#) = happyShift action_58
action_105 (123#) = happyShift action_59
action_105 (124#) = happyShift action_60
action_105 (125#) = happyShift action_19
action_105 (126#) = happyShift action_20
action_105 (130#) = happyShift action_21
action_105 (141#) = happyShift action_61
action_105 (146#) = happyShift action_62
action_105 (147#) = happyShift action_63
action_105 (148#) = happyShift action_64
action_105 (149#) = happyShift action_65
action_105 (150#) = happyShift action_66
action_105 (151#) = happyShift action_67
action_105 (153#) = happyShift action_68
action_105 (15#) = happyGoto action_31
action_105 (33#) = happyGoto action_109
action_105 (34#) = happyGoto action_34
action_105 (35#) = happyGoto action_35
action_105 (36#) = happyGoto action_36
action_105 (37#) = happyGoto action_37
action_105 (41#) = happyGoto action_38
action_105 (42#) = happyGoto action_39
action_105 (43#) = happyGoto action_40
action_105 (44#) = happyGoto action_41
action_105 (45#) = happyGoto action_42
action_105 (46#) = happyGoto action_43
action_105 (47#) = happyGoto action_44
action_105 (48#) = happyGoto action_45
action_105 (49#) = happyGoto action_46
action_105 (50#) = happyGoto action_116
action_105 (51#) = happyGoto action_117
action_105 (52#) = happyGoto action_47
action_105 (59#) = happyGoto action_48
action_105 (61#) = happyGoto action_49
action_105 (62#) = happyGoto action_50
action_105 (63#) = happyGoto action_51
action_105 (85#) = happyGoto action_52
action_105 x = happyTcHack x happyFail

action_106 (93#) = happyShift action_115
action_106 x = happyTcHack x happyFail

action_107 (141#) = happyShift action_27
action_107 (60#) = happyGoto action_113
action_107 (89#) = happyGoto action_114
action_107 x = happyTcHack x happyFail

action_108 (98#) = happyShift action_53
action_108 (118#) = happyShift action_54
action_108 (119#) = happyShift action_55
action_108 (120#) = happyShift action_56
action_108 (121#) = happyShift action_57
action_108 (122#) = happyShift action_58
action_108 (123#) = happyShift action_59
action_108 (124#) = happyShift action_60
action_108 (125#) = happyShift action_19
action_108 (126#) = happyShift action_20
action_108 (130#) = happyShift action_21
action_108 (141#) = happyShift action_61
action_108 (146#) = happyShift action_62
action_108 (147#) = happyShift action_63
action_108 (148#) = happyShift action_64
action_108 (149#) = happyShift action_65
action_108 (150#) = happyShift action_66
action_108 (151#) = happyShift action_67
action_108 (153#) = happyShift action_68
action_108 (15#) = happyGoto action_31
action_108 (33#) = happyGoto action_109
action_108 (34#) = happyGoto action_34
action_108 (35#) = happyGoto action_35
action_108 (36#) = happyGoto action_36
action_108 (37#) = happyGoto action_37
action_108 (41#) = happyGoto action_38
action_108 (42#) = happyGoto action_39
action_108 (43#) = happyGoto action_40
action_108 (44#) = happyGoto action_41
action_108 (45#) = happyGoto action_42
action_108 (46#) = happyGoto action_43
action_108 (47#) = happyGoto action_44
action_108 (48#) = happyGoto action_45
action_108 (49#) = happyGoto action_46
action_108 (51#) = happyGoto action_110
action_108 (52#) = happyGoto action_47
action_108 (59#) = happyGoto action_48
action_108 (61#) = happyGoto action_49
action_108 (62#) = happyGoto action_50
action_108 (63#) = happyGoto action_51
action_108 (71#) = happyGoto action_111
action_108 (72#) = happyGoto action_112
action_108 (85#) = happyGoto action_52
action_108 x = happyTcHack x happyReduce_144

action_109 (141#) = happyShift action_27
action_109 (38#) = happyGoto action_205
action_109 (40#) = happyGoto action_103
action_109 (89#) = happyGoto action_104
action_109 x = happyTcHack x happyFail

action_110 x = happyTcHack x happyReduce_146

action_111 (96#) = happyShift action_204
action_111 x = happyTcHack x happyFail

action_112 (98#) = happyShift action_53
action_112 (118#) = happyShift action_54
action_112 (119#) = happyShift action_55
action_112 (120#) = happyShift action_56
action_112 (121#) = happyShift action_57
action_112 (122#) = happyShift action_58
action_112 (123#) = happyShift action_59
action_112 (124#) = happyShift action_60
action_112 (125#) = happyShift action_19
action_112 (126#) = happyShift action_20
action_112 (130#) = happyShift action_21
action_112 (141#) = happyShift action_61
action_112 (146#) = happyShift action_62
action_112 (147#) = happyShift action_63
action_112 (148#) = happyShift action_64
action_112 (149#) = happyShift action_65
action_112 (150#) = happyShift action_66
action_112 (151#) = happyShift action_67
action_112 (153#) = happyShift action_68
action_112 (15#) = happyGoto action_31
action_112 (33#) = happyGoto action_109
action_112 (34#) = happyGoto action_34
action_112 (35#) = happyGoto action_35
action_112 (36#) = happyGoto action_36
action_112 (37#) = happyGoto action_37
action_112 (41#) = happyGoto action_38
action_112 (42#) = happyGoto action_39
action_112 (43#) = happyGoto action_40
action_112 (44#) = happyGoto action_41
action_112 (45#) = happyGoto action_42
action_112 (46#) = happyGoto action_43
action_112 (47#) = happyGoto action_44
action_112 (48#) = happyGoto action_45
action_112 (49#) = happyGoto action_46
action_112 (51#) = happyGoto action_203
action_112 (52#) = happyGoto action_47
action_112 (59#) = happyGoto action_48
action_112 (61#) = happyGoto action_49
action_112 (62#) = happyGoto action_50
action_112 (63#) = happyGoto action_51
action_112 (85#) = happyGoto action_52
action_112 x = happyTcHack x happyReduce_145

action_113 (96#) = happyShift action_201
action_113 (99#) = happyShift action_202
action_113 x = happyTcHack x happyFail

action_114 x = happyTcHack x happyReduce_126

action_115 (98#) = happyShift action_53
action_115 (119#) = happyShift action_55
action_115 (120#) = happyShift action_56
action_115 (121#) = happyShift action_57
action_115 (122#) = happyShift action_58
action_115 (124#) = happyShift action_60
action_115 (130#) = happyShift action_21
action_115 (141#) = happyShift action_61
action_115 (15#) = happyGoto action_195
action_115 (42#) = happyGoto action_196
action_115 (43#) = happyGoto action_197
action_115 (45#) = happyGoto action_198
action_115 (53#) = happyGoto action_199
action_115 (59#) = happyGoto action_200
action_115 x = happyTcHack x happyFail

action_116 (96#) = happyShift action_194
action_116 (98#) = happyShift action_53
action_116 (118#) = happyShift action_54
action_116 (119#) = happyShift action_55
action_116 (120#) = happyShift action_56
action_116 (121#) = happyShift action_57
action_116 (122#) = happyShift action_58
action_116 (123#) = happyShift action_59
action_116 (124#) = happyShift action_60
action_116 (125#) = happyShift action_19
action_116 (126#) = happyShift action_20
action_116 (130#) = happyShift action_21
action_116 (141#) = happyShift action_61
action_116 (146#) = happyShift action_62
action_116 (147#) = happyShift action_63
action_116 (148#) = happyShift action_64
action_116 (149#) = happyShift action_65
action_116 (150#) = happyShift action_66
action_116 (151#) = happyShift action_67
action_116 (153#) = happyShift action_68
action_116 (15#) = happyGoto action_31
action_116 (33#) = happyGoto action_109
action_116 (34#) = happyGoto action_34
action_116 (35#) = happyGoto action_35
action_116 (36#) = happyGoto action_36
action_116 (37#) = happyGoto action_37
action_116 (41#) = happyGoto action_38
action_116 (42#) = happyGoto action_39
action_116 (43#) = happyGoto action_40
action_116 (44#) = happyGoto action_41
action_116 (45#) = happyGoto action_42
action_116 (46#) = happyGoto action_43
action_116 (47#) = happyGoto action_44
action_116 (48#) = happyGoto action_45
action_116 (49#) = happyGoto action_46
action_116 (51#) = happyGoto action_193
action_116 (52#) = happyGoto action_47
action_116 (59#) = happyGoto action_48
action_116 (61#) = happyGoto action_49
action_116 (62#) = happyGoto action_50
action_116 (63#) = happyGoto action_51
action_116 (85#) = happyGoto action_52
action_116 x = happyTcHack x happyFail

action_117 x = happyTcHack x happyReduce_108

action_118 (135#) = happyShift action_120
action_118 (66#) = happyGoto action_192
action_118 x = happyTcHack x happyReduce_96

action_119 x = happyTcHack x happyReduce_135

action_120 (98#) = happyShift action_53
action_120 (113#) = happyShift action_135
action_120 (139#) = happyShift action_136
action_120 (141#) = happyShift action_61
action_120 (143#) = happyShift action_137
action_120 (145#) = happyShift action_138
action_120 (15#) = happyGoto action_122
action_120 (19#) = happyGoto action_123
action_120 (20#) = happyGoto action_124
action_120 (21#) = happyGoto action_125
action_120 (22#) = happyGoto action_126
action_120 (23#) = happyGoto action_127
action_120 (24#) = happyGoto action_128
action_120 (25#) = happyGoto action_129
action_120 (26#) = happyGoto action_130
action_120 (27#) = happyGoto action_131
action_120 (28#) = happyGoto action_132
action_120 (29#) = happyGoto action_133
action_120 (30#) = happyGoto action_191
action_120 x = happyTcHack x happyFail

action_121 (141#) = happyShift action_27
action_121 (40#) = happyGoto action_190
action_121 (89#) = happyGoto action_104
action_121 x = happyTcHack x happyFail

action_122 (98#) = happyShift action_94
action_122 x = happyTcHack x happyReduce_63

action_123 x = happyTcHack x happyReduce_66

action_124 (105#) = happyShift action_189
action_124 x = happyTcHack x happyReduce_42

action_125 (107#) = happyShift action_188
action_125 x = happyTcHack x happyReduce_43

action_126 (108#) = happyShift action_187
action_126 x = happyTcHack x happyReduce_45

action_127 (110#) = happyShift action_186
action_127 x = happyTcHack x happyReduce_47

action_128 (143#) = happyShift action_184
action_128 (145#) = happyShift action_185
action_128 x = happyTcHack x happyReduce_49

action_129 (111#) = happyShift action_181
action_129 (112#) = happyShift action_182
action_129 (144#) = happyShift action_183
action_129 x = happyTcHack x happyReduce_51

action_130 x = happyTcHack x happyReduce_54

action_131 (98#) = happyShift action_53
action_131 (139#) = happyShift action_136
action_131 (141#) = happyShift action_61
action_131 (15#) = happyGoto action_122
action_131 (28#) = happyGoto action_180
action_131 (29#) = happyGoto action_133
action_131 x = happyTcHack x happyFail

action_132 x = happyTcHack x happyReduce_59

action_133 x = happyTcHack x happyReduce_64

action_134 (133#) = happyShift action_179
action_134 x = happyTcHack x happyFail

action_135 x = happyTcHack x happyReduce_62

action_136 x = happyTcHack x happyReduce_65

action_137 x = happyTcHack x happyReduce_61

action_138 x = happyTcHack x happyReduce_60

action_139 (133#) = happyShift action_178
action_139 x = happyTcHack x happyFail

action_140 (99#) = happyShift action_176
action_140 (133#) = happyShift action_177
action_140 x = happyTcHack x happyFail

action_141 (99#) = happyShift action_175
action_141 x = happyTcHack x happyFail

action_142 x = happyTcHack x happyReduce_29

action_143 (98#) = happyShift action_53
action_143 (113#) = happyShift action_135
action_143 (139#) = happyShift action_136
action_143 (141#) = happyShift action_61
action_143 (143#) = happyShift action_137
action_143 (145#) = happyShift action_138
action_143 (15#) = happyGoto action_122
action_143 (19#) = happyGoto action_174
action_143 (20#) = happyGoto action_124
action_143 (21#) = happyGoto action_125
action_143 (22#) = happyGoto action_126
action_143 (23#) = happyGoto action_127
action_143 (24#) = happyGoto action_128
action_143 (25#) = happyGoto action_129
action_143 (26#) = happyGoto action_130
action_143 (27#) = happyGoto action_131
action_143 (28#) = happyGoto action_132
action_143 (29#) = happyGoto action_133
action_143 x = happyTcHack x happyFail

action_144 (98#) = happyShift action_94
action_144 (99#) = happyShift action_173
action_144 (16#) = happyGoto action_172
action_144 x = happyTcHack x happyReduce_30

action_145 (91#) = happyShift action_15
action_145 (92#) = happyShift action_16
action_145 (96#) = happyShift action_171
action_145 (101#) = happyShift action_17
action_145 (116#) = happyShift action_18
action_145 (125#) = happyShift action_19
action_145 (126#) = happyShift action_20
action_145 (130#) = happyShift action_21
action_145 (154#) = happyShift action_22
action_145 (158#) = happyShift action_23
action_145 (159#) = happyShift action_24
action_145 (160#) = happyShift action_25
action_145 (6#) = happyGoto action_4
action_145 (7#) = happyGoto action_5
action_145 (8#) = happyGoto action_6
action_145 (9#) = happyGoto action_7
action_145 (10#) = happyGoto action_8
action_145 (17#) = happyGoto action_9
action_145 (31#) = happyGoto action_10
action_145 (49#) = happyGoto action_11
action_145 (52#) = happyGoto action_12
action_145 (59#) = happyGoto action_13
action_145 (70#) = happyGoto action_14
action_145 x = happyTcHack x happyFail

action_146 x = happyTcHack x happyReduce_18

action_147 (90#) = happyShift action_170
action_147 x = happyTcHack x happyFail

action_148 (90#) = happyShift action_169
action_148 x = happyTcHack x happyFail

action_149 (90#) = happyShift action_168
action_149 x = happyTcHack x happyFail

action_150 (142#) = happyShift action_167
action_150 x = happyTcHack x happyFail

action_151 (90#) = happyShift action_166
action_151 x = happyTcHack x happyFail

action_152 (90#) = happyShift action_165
action_152 x = happyTcHack x happyFail

action_153 (98#) = happyShift action_53
action_153 (118#) = happyShift action_54
action_153 (119#) = happyShift action_55
action_153 (120#) = happyShift action_56
action_153 (121#) = happyShift action_57
action_153 (122#) = happyShift action_58
action_153 (123#) = happyShift action_59
action_153 (124#) = happyShift action_60
action_153 (137#) = happyShift action_164
action_153 (141#) = happyShift action_61
action_153 (146#) = happyShift action_62
action_153 (147#) = happyShift action_63
action_153 (149#) = happyShift action_65
action_153 (150#) = happyShift action_66
action_153 (151#) = happyShift action_67
action_153 (153#) = happyShift action_68
action_153 (15#) = happyGoto action_157
action_153 (35#) = happyGoto action_158
action_153 (41#) = happyGoto action_38
action_153 (42#) = happyGoto action_39
action_153 (43#) = happyGoto action_40
action_153 (44#) = happyGoto action_41
action_153 (45#) = happyGoto action_42
action_153 (46#) = happyGoto action_43
action_153 (47#) = happyGoto action_44
action_153 (48#) = happyGoto action_45
action_153 (62#) = happyGoto action_159
action_153 (63#) = happyGoto action_160
action_153 (75#) = happyGoto action_161
action_153 (84#) = happyGoto action_162
action_153 (85#) = happyGoto action_163
action_153 x = happyTcHack x happyFail

action_154 x = happyTcHack x happyReduce_15

action_155 x = happyTcHack x happyReduce_150

action_156 x = happyTcHack x happyReduce_139

action_157 (98#) = happyShift action_94
action_157 x = happyTcHack x happyReduce_171

action_158 x = happyTcHack x happyReduce_167

action_159 x = happyTcHack x happyReduce_168

action_160 x = happyTcHack x happyReduce_169

action_161 (141#) = happyShift action_27
action_161 (89#) = happyGoto action_224
action_161 x = happyTcHack x happyFail

action_162 x = happyTcHack x happyReduce_151

action_163 x = happyTcHack x happyReduce_170

action_164 x = happyTcHack x happyReduce_152

action_165 x = happyTcHack x happyReduce_23

action_166 x = happyTcHack x happyReduce_21

action_167 (98#) = happyShift action_53
action_167 (118#) = happyShift action_54
action_167 (119#) = happyShift action_55
action_167 (120#) = happyShift action_56
action_167 (121#) = happyShift action_57
action_167 (122#) = happyShift action_58
action_167 (123#) = happyShift action_59
action_167 (124#) = happyShift action_60
action_167 (141#) = happyShift action_61
action_167 (146#) = happyShift action_62
action_167 (147#) = happyShift action_63
action_167 (149#) = happyShift action_65
action_167 (150#) = happyShift action_66
action_167 (151#) = happyShift action_67
action_167 (153#) = happyShift action_68
action_167 (15#) = happyGoto action_157
action_167 (35#) = happyGoto action_158
action_167 (41#) = happyGoto action_38
action_167 (42#) = happyGoto action_39
action_167 (43#) = happyGoto action_40
action_167 (44#) = happyGoto action_41
action_167 (45#) = happyGoto action_42
action_167 (46#) = happyGoto action_43
action_167 (47#) = happyGoto action_44
action_167 (48#) = happyGoto action_45
action_167 (62#) = happyGoto action_159
action_167 (63#) = happyGoto action_160
action_167 (84#) = happyGoto action_223
action_167 (85#) = happyGoto action_163
action_167 x = happyTcHack x happyFail

action_168 x = happyTcHack x happyReduce_22

action_169 x = happyTcHack x happyReduce_19

action_170 x = happyTcHack x happyReduce_20

action_171 x = happyTcHack x happyReduce_12

action_172 x = happyTcHack x happyReduce_26

action_173 (98#) = happyShift action_53
action_173 (141#) = happyShift action_61
action_173 (15#) = happyGoto action_222
action_173 x = happyTcHack x happyFail

action_174 x = happyTcHack x happyReduce_32

action_175 (139#) = happyShift action_221
action_175 (87#) = happyGoto action_220
action_175 x = happyTcHack x happyFail

action_176 (98#) = happyShift action_53
action_176 (113#) = happyShift action_135
action_176 (139#) = happyShift action_136
action_176 (141#) = happyShift action_61
action_176 (143#) = happyShift action_137
action_176 (145#) = happyShift action_138
action_176 (15#) = happyGoto action_122
action_176 (19#) = happyGoto action_123
action_176 (20#) = happyGoto action_124
action_176 (21#) = happyGoto action_125
action_176 (22#) = happyGoto action_126
action_176 (23#) = happyGoto action_127
action_176 (24#) = happyGoto action_128
action_176 (25#) = happyGoto action_129
action_176 (26#) = happyGoto action_130
action_176 (27#) = happyGoto action_131
action_176 (28#) = happyGoto action_132
action_176 (29#) = happyGoto action_133
action_176 (30#) = happyGoto action_219
action_176 x = happyTcHack x happyFail

action_177 x = happyTcHack x happyReduce_129

action_178 x = happyTcHack x happyReduce_132

action_179 x = happyTcHack x happyReduce_130

action_180 x = happyTcHack x happyReduce_58

action_181 (98#) = happyShift action_53
action_181 (113#) = happyShift action_135
action_181 (139#) = happyShift action_136
action_181 (141#) = happyShift action_61
action_181 (143#) = happyShift action_137
action_181 (145#) = happyShift action_138
action_181 (15#) = happyGoto action_122
action_181 (26#) = happyGoto action_218
action_181 (27#) = happyGoto action_131
action_181 (28#) = happyGoto action_132
action_181 (29#) = happyGoto action_133
action_181 x = happyTcHack x happyFail

action_182 (98#) = happyShift action_53
action_182 (113#) = happyShift action_135
action_182 (139#) = happyShift action_136
action_182 (141#) = happyShift action_61
action_182 (143#) = happyShift action_137
action_182 (145#) = happyShift action_138
action_182 (15#) = happyGoto action_122
action_182 (26#) = happyGoto action_217
action_182 (27#) = happyGoto action_131
action_182 (28#) = happyGoto action_132
action_182 (29#) = happyGoto action_133
action_182 x = happyTcHack x happyFail

action_183 (98#) = happyShift action_53
action_183 (113#) = happyShift action_135
action_183 (139#) = happyShift action_136
action_183 (141#) = happyShift action_61
action_183 (143#) = happyShift action_137
action_183 (145#) = happyShift action_138
action_183 (15#) = happyGoto action_122
action_183 (26#) = happyGoto action_216
action_183 (27#) = happyGoto action_131
action_183 (28#) = happyGoto action_132
action_183 (29#) = happyGoto action_133
action_183 x = happyTcHack x happyFail

action_184 (98#) = happyShift action_53
action_184 (113#) = happyShift action_135
action_184 (139#) = happyShift action_136
action_184 (141#) = happyShift action_61
action_184 (143#) = happyShift action_137
action_184 (145#) = happyShift action_138
action_184 (15#) = happyGoto action_122
action_184 (25#) = happyGoto action_215
action_184 (26#) = happyGoto action_130
action_184 (27#) = happyGoto action_131
action_184 (28#) = happyGoto action_132
action_184 (29#) = happyGoto action_133
action_184 x = happyTcHack x happyFail

action_185 (98#) = happyShift action_53
action_185 (113#) = happyShift action_135
action_185 (139#) = happyShift action_136
action_185 (141#) = happyShift action_61
action_185 (143#) = happyShift action_137
action_185 (145#) = happyShift action_138
action_185 (15#) = happyGoto action_122
action_185 (25#) = happyGoto action_214
action_185 (26#) = happyGoto action_130
action_185 (27#) = happyGoto action_131
action_185 (28#) = happyGoto action_132
action_185 (29#) = happyGoto action_133
action_185 x = happyTcHack x happyFail

action_186 (98#) = happyShift action_53
action_186 (113#) = happyShift action_135
action_186 (139#) = happyShift action_136
action_186 (141#) = happyShift action_61
action_186 (143#) = happyShift action_137
action_186 (145#) = happyShift action_138
action_186 (15#) = happyGoto action_122
action_186 (24#) = happyGoto action_213
action_186 (25#) = happyGoto action_129
action_186 (26#) = happyGoto action_130
action_186 (27#) = happyGoto action_131
action_186 (28#) = happyGoto action_132
action_186 (29#) = happyGoto action_133
action_186 x = happyTcHack x happyFail

action_187 (98#) = happyShift action_53
action_187 (113#) = happyShift action_135
action_187 (139#) = happyShift action_136
action_187 (141#) = happyShift action_61
action_187 (143#) = happyShift action_137
action_187 (145#) = happyShift action_138
action_187 (15#) = happyGoto action_122
action_187 (23#) = happyGoto action_212
action_187 (24#) = happyGoto action_128
action_187 (25#) = happyGoto action_129
action_187 (26#) = happyGoto action_130
action_187 (27#) = happyGoto action_131
action_187 (28#) = happyGoto action_132
action_187 (29#) = happyGoto action_133
action_187 x = happyTcHack x happyFail

action_188 (98#) = happyShift action_53
action_188 (113#) = happyShift action_135
action_188 (139#) = happyShift action_136
action_188 (141#) = happyShift action_61
action_188 (143#) = happyShift action_137
action_188 (145#) = happyShift action_138
action_188 (15#) = happyGoto action_122
action_188 (22#) = happyGoto action_211
action_188 (23#) = happyGoto action_127
action_188 (24#) = happyGoto action_128
action_188 (25#) = happyGoto action_129
action_188 (26#) = happyGoto action_130
action_188 (27#) = happyGoto action_131
action_188 (28#) = happyGoto action_132
action_188 (29#) = happyGoto action_133
action_188 x = happyTcHack x happyFail

action_189 (98#) = happyShift action_53
action_189 (113#) = happyShift action_135
action_189 (139#) = happyShift action_136
action_189 (141#) = happyShift action_61
action_189 (143#) = happyShift action_137
action_189 (145#) = happyShift action_138
action_189 (15#) = happyGoto action_122
action_189 (21#) = happyGoto action_210
action_189 (22#) = happyGoto action_126
action_189 (23#) = happyGoto action_127
action_189 (24#) = happyGoto action_128
action_189 (25#) = happyGoto action_129
action_189 (26#) = happyGoto action_130
action_189 (27#) = happyGoto action_131
action_189 (28#) = happyGoto action_132
action_189 (29#) = happyGoto action_133
action_189 x = happyTcHack x happyFail

action_190 x = happyTcHack x happyReduce_93

action_191 (136#) = happyShift action_209
action_191 x = happyTcHack x happyFail

action_192 x = happyTcHack x happyReduce_136

action_193 x = happyTcHack x happyReduce_109

action_194 x = happyTcHack x happyReduce_107

action_195 (98#) = happyShift action_94
action_195 x = happyTcHack x happyReduce_116

action_196 x = happyTcHack x happyReduce_112

action_197 x = happyTcHack x happyReduce_113

action_198 x = happyTcHack x happyReduce_114

action_199 (94#) = happyShift action_208
action_199 x = happyTcHack x happyFail

action_200 x = happyTcHack x happyReduce_115

action_201 x = happyTcHack x happyReduce_125

action_202 (141#) = happyShift action_27
action_202 (89#) = happyGoto action_207
action_202 x = happyTcHack x happyFail

action_203 x = happyTcHack x happyReduce_147

action_204 x = happyTcHack x happyReduce_143

action_205 (90#) = happyShift action_206
action_205 (99#) = happyShift action_121
action_205 x = happyTcHack x happyFail

action_206 x = happyTcHack x happyReduce_110

action_207 x = happyTcHack x happyReduce_127

action_208 (95#) = happyShift action_233
action_208 x = happyTcHack x happyFail

action_209 x = happyTcHack x happyReduce_137

action_210 (107#) = happyShift action_188
action_210 x = happyTcHack x happyReduce_44

action_211 (108#) = happyShift action_187
action_211 x = happyTcHack x happyReduce_46

action_212 (110#) = happyShift action_186
action_212 x = happyTcHack x happyReduce_48

action_213 (143#) = happyShift action_184
action_213 (145#) = happyShift action_185
action_213 x = happyTcHack x happyReduce_50

action_214 (111#) = happyShift action_181
action_214 (112#) = happyShift action_182
action_214 (144#) = happyShift action_183
action_214 x = happyTcHack x happyReduce_53

action_215 (111#) = happyShift action_181
action_215 (112#) = happyShift action_182
action_215 (144#) = happyShift action_183
action_215 x = happyTcHack x happyReduce_52

action_216 x = happyTcHack x happyReduce_55

action_217 x = happyTcHack x happyReduce_57

action_218 x = happyTcHack x happyReduce_56

action_219 (133#) = happyShift action_232
action_219 x = happyTcHack x happyFail

action_220 (133#) = happyShift action_231
action_220 x = happyTcHack x happyFail

action_221 x = happyTcHack x happyReduce_174

action_222 (98#) = happyShift action_94
action_222 (99#) = happyShift action_173
action_222 (16#) = happyGoto action_230
action_222 x = happyTcHack x happyReduce_30

action_223 (141#) = happyShift action_27
action_223 (39#) = happyGoto action_227
action_223 (69#) = happyGoto action_228
action_223 (89#) = happyGoto action_229
action_223 x = happyTcHack x happyFail

action_224 (93#) = happyShift action_226
action_224 (76#) = happyGoto action_225
action_224 x = happyTcHack x happyFail

action_225 (155#) = happyShift action_247
action_225 (80#) = happyGoto action_246
action_225 x = happyTcHack x happyReduce_159

action_226 (94#) = happyShift action_244
action_226 (138#) = happyShift action_245
action_226 (77#) = happyGoto action_241
action_226 (78#) = happyGoto action_242
action_226 (79#) = happyGoto action_243
action_226 x = happyTcHack x happyFail

action_227 x = happyTcHack x happyReduce_141

action_228 (99#) = happyShift action_240
action_228 x = happyTcHack x happyReduce_138

action_229 x = happyTcHack x happyReduce_94

action_230 x = happyTcHack x happyReduce_31

action_231 x = happyTcHack x happyReduce_172

action_232 x = happyTcHack x happyReduce_128

action_233 (128#) = happyShift action_238
action_233 (129#) = happyShift action_239
action_233 (54#) = happyGoto action_234
action_233 (55#) = happyGoto action_235
action_233 (56#) = happyGoto action_236
action_233 (57#) = happyGoto action_237
action_233 x = happyTcHack x happyFail

action_234 (96#) = happyShift action_261
action_234 (128#) = happyShift action_238
action_234 (129#) = happyShift action_239
action_234 (55#) = happyGoto action_260
action_234 (56#) = happyGoto action_236
action_234 (57#) = happyGoto action_237
action_234 x = happyTcHack x happyFail

action_235 x = happyTcHack x happyReduce_117

action_236 (98#) = happyShift action_53
action_236 (118#) = happyShift action_54
action_236 (119#) = happyShift action_55
action_236 (120#) = happyShift action_56
action_236 (121#) = happyShift action_57
action_236 (122#) = happyShift action_58
action_236 (123#) = happyShift action_59
action_236 (124#) = happyShift action_60
action_236 (125#) = happyShift action_19
action_236 (126#) = happyShift action_20
action_236 (128#) = happyShift action_238
action_236 (129#) = happyShift action_239
action_236 (130#) = happyShift action_21
action_236 (141#) = happyShift action_61
action_236 (146#) = happyShift action_62
action_236 (147#) = happyShift action_63
action_236 (148#) = happyShift action_64
action_236 (149#) = happyShift action_65
action_236 (150#) = happyShift action_66
action_236 (151#) = happyShift action_67
action_236 (153#) = happyShift action_68
action_236 (15#) = happyGoto action_31
action_236 (33#) = happyGoto action_257
action_236 (34#) = happyGoto action_34
action_236 (35#) = happyGoto action_35
action_236 (36#) = happyGoto action_36
action_236 (37#) = happyGoto action_37
action_236 (41#) = happyGoto action_38
action_236 (42#) = happyGoto action_39
action_236 (43#) = happyGoto action_40
action_236 (44#) = happyGoto action_41
action_236 (45#) = happyGoto action_42
action_236 (46#) = happyGoto action_43
action_236 (47#) = happyGoto action_44
action_236 (48#) = happyGoto action_45
action_236 (49#) = happyGoto action_46
action_236 (52#) = happyGoto action_47
action_236 (57#) = happyGoto action_258
action_236 (58#) = happyGoto action_259
action_236 (59#) = happyGoto action_48
action_236 (61#) = happyGoto action_49
action_236 (62#) = happyGoto action_50
action_236 (63#) = happyGoto action_51
action_236 (85#) = happyGoto action_52
action_236 x = happyTcHack x happyFail

action_237 x = happyTcHack x happyReduce_120

action_238 (98#) = happyShift action_53
action_238 (113#) = happyShift action_135
action_238 (139#) = happyShift action_136
action_238 (141#) = happyShift action_61
action_238 (143#) = happyShift action_137
action_238 (145#) = happyShift action_138
action_238 (15#) = happyGoto action_122
action_238 (19#) = happyGoto action_256
action_238 (20#) = happyGoto action_124
action_238 (21#) = happyGoto action_125
action_238 (22#) = happyGoto action_126
action_238 (23#) = happyGoto action_127
action_238 (24#) = happyGoto action_128
action_238 (25#) = happyGoto action_129
action_238 (26#) = happyGoto action_130
action_238 (27#) = happyGoto action_131
action_238 (28#) = happyGoto action_132
action_238 (29#) = happyGoto action_133
action_238 x = happyTcHack x happyFail

action_239 (97#) = happyShift action_255
action_239 x = happyTcHack x happyFail

action_240 (141#) = happyShift action_27
action_240 (39#) = happyGoto action_254
action_240 (89#) = happyGoto action_229
action_240 x = happyTcHack x happyFail

action_241 (94#) = happyShift action_252
action_241 (99#) = happyShift action_253
action_241 x = happyTcHack x happyFail

action_242 x = happyTcHack x happyReduce_155

action_243 (98#) = happyShift action_53
action_243 (118#) = happyShift action_54
action_243 (119#) = happyShift action_55
action_243 (120#) = happyShift action_56
action_243 (121#) = happyShift action_57
action_243 (122#) = happyShift action_58
action_243 (123#) = happyShift action_59
action_243 (124#) = happyShift action_60
action_243 (141#) = happyShift action_61
action_243 (146#) = happyShift action_62
action_243 (147#) = happyShift action_63
action_243 (149#) = happyShift action_65
action_243 (150#) = happyShift action_66
action_243 (151#) = happyShift action_67
action_243 (153#) = happyShift action_68
action_243 (15#) = happyGoto action_157
action_243 (35#) = happyGoto action_158
action_243 (41#) = happyGoto action_38
action_243 (42#) = happyGoto action_39
action_243 (43#) = happyGoto action_40
action_243 (44#) = happyGoto action_41
action_243 (45#) = happyGoto action_42
action_243 (46#) = happyGoto action_43
action_243 (47#) = happyGoto action_44
action_243 (48#) = happyGoto action_45
action_243 (62#) = happyGoto action_159
action_243 (63#) = happyGoto action_160
action_243 (84#) = happyGoto action_251
action_243 (85#) = happyGoto action_163
action_243 x = happyTcHack x happyFail

action_244 x = happyTcHack x happyReduce_154

action_245 x = happyTcHack x happyReduce_158

action_246 (156#) = happyShift action_250
action_246 (81#) = happyGoto action_249
action_246 x = happyTcHack x happyReduce_161

action_247 (93#) = happyShift action_248
action_247 x = happyTcHack x happyFail

action_248 (98#) = happyShift action_53
action_248 (141#) = happyShift action_61
action_248 (15#) = happyGoto action_268
action_248 (82#) = happyGoto action_269
action_248 x = happyTcHack x happyFail

action_249 x = happyTcHack x happyReduce_148

action_250 (93#) = happyShift action_267
action_250 x = happyTcHack x happyFail

action_251 (141#) = happyShift action_27
action_251 (39#) = happyGoto action_266
action_251 (89#) = happyGoto action_229
action_251 x = happyTcHack x happyFail

action_252 x = happyTcHack x happyReduce_153

action_253 (138#) = happyShift action_245
action_253 (78#) = happyGoto action_265
action_253 (79#) = happyGoto action_243
action_253 x = happyTcHack x happyFail

action_254 x = happyTcHack x happyReduce_142

action_255 x = happyTcHack x happyReduce_123

action_256 (97#) = happyShift action_264
action_256 x = happyTcHack x happyFail

action_257 (141#) = happyShift action_27
action_257 (40#) = happyGoto action_263
action_257 (89#) = happyGoto action_104
action_257 x = happyTcHack x happyFail

action_258 x = happyTcHack x happyReduce_121

action_259 (90#) = happyShift action_262
action_259 x = happyTcHack x happyFail

action_260 x = happyTcHack x happyReduce_118

action_261 x = happyTcHack x happyReduce_111

action_262 x = happyTcHack x happyReduce_119

action_263 x = happyTcHack x happyReduce_124

action_264 x = happyTcHack x happyReduce_122

action_265 x = happyTcHack x happyReduce_156

action_266 x = happyTcHack x happyReduce_157

action_267 (140#) = happyShift action_273
action_267 (83#) = happyGoto action_272
action_267 x = happyTcHack x happyFail

action_268 (98#) = happyShift action_94
action_268 x = happyTcHack x happyReduce_163

action_269 (94#) = happyShift action_270
action_269 (99#) = happyShift action_271
action_269 x = happyTcHack x happyFail

action_270 x = happyTcHack x happyReduce_160

action_271 (98#) = happyShift action_53
action_271 (141#) = happyShift action_61
action_271 (15#) = happyGoto action_276
action_271 x = happyTcHack x happyFail

action_272 (94#) = happyShift action_274
action_272 (99#) = happyShift action_275
action_272 x = happyTcHack x happyFail

action_273 x = happyTcHack x happyReduce_165

action_274 x = happyTcHack x happyReduce_162

action_275 (140#) = happyShift action_277
action_275 x = happyTcHack x happyFail

action_276 (98#) = happyShift action_94
action_276 x = happyTcHack x happyReduce_164

action_277 x = happyTcHack x happyReduce_166

happyReduce_1 = happySpecReduce_1  4# happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 ((reverse happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5# happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5# happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  6# happyReduction_4
happyReduction_4 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6# happyReduction_5
happyReduction_5 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6# happyReduction_6
happyReduction_6 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6# happyReduction_7
happyReduction_7 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  6# happyReduction_8
happyReduction_8 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6# happyReduction_9
happyReduction_9 (HappyTerminal (T_pragma happy_var_1))
	 =  HappyAbsSyn6
		 (Pragma happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6# happyReduction_10
happyReduction_10 (HappyTerminal (T_include_start happy_var_1))
	 =  HappyAbsSyn6
		 (IncludeStart happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6# happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn6
		 (IncludeEnd
	)

happyReduce_12 = happyReduce 5# 7# happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Module happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  8# happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  8# happyReduction_14
happyReduction_14 (HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Forward happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4# 9# happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (let (ids,inherit) = happy_var_1 in Interface ids inherit (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  10# happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn39  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ((happy_var_2,happy_var_3)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  11# happyReduction_17
happyReduction_17  =  HappyAbsSyn4
		 ([]
	)

happyReduce_18 = happySpecReduce_2  11# happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  12# happyReduction_19
happyReduction_19 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  12# happyReduction_20
happyReduction_20 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  12# happyReduction_21
happyReduction_21 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  12# happyReduction_22
happyReduction_22 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  12# happyReduction_23
happyReduction_23 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  13# happyReduction_24
happyReduction_24  =  HappyAbsSyn13
		 ([]
	)

happyReduce_25 = happySpecReduce_1  13# happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  14# happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2:(reverse happy_var_3)
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  15# happyReduction_27
happyReduction_27 (HappyTerminal (T_id happy_var_1))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  15# happyReduction_28
happyReduction_28 (HappyTerminal (T_id happy_var_2))
	_
	 =  HappyAbsSyn15
		 (("::"++ happy_var_2)
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  15# happyReduction_29
happyReduction_29 (HappyTerminal (T_id happy_var_3))
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ ':':':':happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_0  16# happyReduction_30
happyReduction_30  =  HappyAbsSyn13
		 ([]
	)

happyReduce_31 = happySpecReduce_3  16# happyReduction_31
happyReduction_31 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 5# 17# happyReduction_32
happyReduction_32 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Constant happy_var_3 [] happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_1  18# happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  18# happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  18# happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  18# happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  18# happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  18# happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  18# happyReduction_39
happyReduction_39 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  18# happyReduction_40
happyReduction_40 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  18# happyReduction_41
happyReduction_41 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (TyName happy_var_1 Nothing
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  19# happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  20# happyReduction_43
happyReduction_43 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  20# happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary Or happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  21# happyReduction_45
happyReduction_45 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  21# happyReduction_46
happyReduction_46 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary Xor happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  22# happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  22# happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary And happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  23# happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  23# happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_3)
	(HappyTerminal (T_shift happy_var_2))
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary (Shift happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  24# happyReduction_51
happyReduction_51 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  24# happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary Add happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24# happyReduction_53
happyReduction_53 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary Sub happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  25# happyReduction_54
happyReduction_54 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  25# happyReduction_55
happyReduction_55 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary Mul happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25# happyReduction_56
happyReduction_56 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary Div happy_var_1 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  25# happyReduction_57
happyReduction_57 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Binary Mod happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  26# happyReduction_58
happyReduction_58 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn19
		 (Unary happy_var_1 happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  26# happyReduction_59
happyReduction_59 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  27# happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn27
		 (Minus
	)

happyReduce_61 = happySpecReduce_1  27# happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn27
		 (Plus
	)

happyReduce_62 = happySpecReduce_1  27# happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn27
		 (Not
	)

happyReduce_63 = happySpecReduce_1  28# happyReduction_63
happyReduction_63 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn19
		 (Var happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  28# happyReduction_64
happyReduction_64 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn19
		 (Lit happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  29# happyReduction_65
happyReduction_65 (HappyTerminal (T_literal happy_var_1))
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  30# happyReduction_66
happyReduction_66 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_2  31# happyReduction_67
happyReduction_67 (HappyAbsSyn32  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (let (spec, decls) = happy_var_2 in Typedef spec [] decls
	)
happyReduction_67 _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  31# happyReduction_68
happyReduction_68 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn6
		 (TypeDecl happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  31# happyReduction_69
happyReduction_69 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn6
		 (TypeDecl happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  31# happyReduction_70
happyReduction_70 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn6
		 (TypeDecl happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_2  32# happyReduction_71
happyReduction_71 (HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn32
		 ((happy_var_1,happy_var_2)
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  33# happyReduction_72
happyReduction_72 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  33# happyReduction_73
happyReduction_73 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  34# happyReduction_74
happyReduction_74 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  34# happyReduction_75
happyReduction_75 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  34# happyReduction_76
happyReduction_76 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (TyName happy_var_1 Nothing
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  35# happyReduction_77
happyReduction_77 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  35# happyReduction_78
happyReduction_78 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  35# happyReduction_79
happyReduction_79 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  35# happyReduction_80
happyReduction_80 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  35# happyReduction_81
happyReduction_81 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  35# happyReduction_82
happyReduction_82 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  35# happyReduction_83
happyReduction_83 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  35# happyReduction_84
happyReduction_84 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  36# happyReduction_85
happyReduction_85 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  36# happyReduction_86
happyReduction_86 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  36# happyReduction_87
happyReduction_87 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  36# happyReduction_88
happyReduction_88 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  37# happyReduction_89
happyReduction_89 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  37# happyReduction_90
happyReduction_90 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  37# happyReduction_91
happyReduction_91 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  38# happyReduction_92
happyReduction_92 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn38
		 ([happy_var_1]
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  38# happyReduction_93
happyReduction_93 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_3 : happy_var_1
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  39# happyReduction_94
happyReduction_94 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  40# happyReduction_95
happyReduction_95 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_2  40# happyReduction_96
happyReduction_96 (HappyAbsSyn65  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (ArrayId happy_var_1 happy_var_2
	)
happyReduction_96 _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_1  41# happyReduction_97
happyReduction_97 (HappyTerminal (T_float happy_var_1))
	 =  HappyAbsSyn18
		 (TyFloat happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  42# happyReduction_98
happyReduction_98 (HappyTerminal (T_int happy_var_1))
	 =  HappyAbsSyn18
		 (TyInteger happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_2  42# happyReduction_99
happyReduction_99 (HappyTerminal (T_int happy_var_2))
	_
	 =  HappyAbsSyn18
		 (TyApply (TySigned True)  (TyInteger happy_var_2)
	)
happyReduction_99 _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_2  42# happyReduction_100
happyReduction_100 (HappyTerminal (T_int happy_var_2))
	_
	 =  HappyAbsSyn18
		 (TyApply (TySigned False) (TyInteger happy_var_2)
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  43# happyReduction_101
happyReduction_101 _
	 =  HappyAbsSyn18
		 (TyChar
	)

happyReduce_102 = happySpecReduce_1  44# happyReduction_102
happyReduction_102 _
	 =  HappyAbsSyn18
		 (TyWChar
	)

happyReduce_103 = happySpecReduce_1  45# happyReduction_103
happyReduction_103 _
	 =  HappyAbsSyn18
		 (TyBool
	)

happyReduce_104 = happySpecReduce_1  46# happyReduction_104
happyReduction_104 _
	 =  HappyAbsSyn18
		 (TyOctet
	)

happyReduce_105 = happySpecReduce_1  47# happyReduction_105
happyReduction_105 _
	 =  HappyAbsSyn18
		 (TyAny
	)

happyReduce_106 = happySpecReduce_1  48# happyReduction_106
happyReduction_106 _
	 =  HappyAbsSyn18
		 (TyObject
	)

happyReduce_107 = happyReduce 5# 49# happyReduction_107
happyReduction_107 (_ `HappyStk`
	(HappyAbsSyn50  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TyStruct (Just happy_var_2) happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_108 = happySpecReduce_1  50# happyReduction_108
happyReduction_108 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn50
		 ([happy_var_1]
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_2  50# happyReduction_109
happyReduction_109 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_2:happy_var_1
	)
happyReduction_109 _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  51# happyReduction_110
happyReduction_110 _
	(HappyAbsSyn38  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn51
		 ((happy_var_1,[],happy_var_2)
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happyReduce 9# 52# happyReduction_111
happyReduction_111 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TyUnion (Just happy_var_2) happy_var_5 (Id "tagged_union") Nothing (reverse happy_var_8)
	) `HappyStk` happyRest

happyReduce_112 = happySpecReduce_1  53# happyReduction_112
happyReduction_112 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_1  53# happyReduction_113
happyReduction_113 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_113 _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  53# happyReduction_114
happyReduction_114 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_1  53# happyReduction_115
happyReduction_115 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_115 _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  53# happyReduction_116
happyReduction_116 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (TyName happy_var_1 Nothing
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  54# happyReduction_117
happyReduction_117 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn54
		 ([happy_var_1]
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_2  54# happyReduction_118
happyReduction_118 (HappyAbsSyn55  happy_var_2)
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_2:happy_var_1
	)
happyReduction_118 _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_3  55# happyReduction_119
happyReduction_119 _
	(HappyAbsSyn58  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn55
		 (Switch happy_var_1 (Just happy_var_2)
	)
happyReduction_119 _ _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  56# happyReduction_120
happyReduction_120 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn56
		 ([happy_var_1]
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_2  56# happyReduction_121
happyReduction_121 (HappyAbsSyn57  happy_var_2)
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_2:happy_var_1
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_3  57# happyReduction_122
happyReduction_122 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn57
		 (Case [happy_var_2]
	)
happyReduction_122 _ _ _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_2  57# happyReduction_123
happyReduction_123 _
	_
	 =  HappyAbsSyn57
		 (Default
	)

happyReduce_124 = happySpecReduce_2  58# happyReduction_124
happyReduction_124 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn58
		 ((Param happy_var_2 happy_var_1 [])
	)
happyReduction_124 _ _  = notHappyAtAll 

happyReduce_125 = happyReduce 5# 59# happyReduction_125
happyReduction_125 (_ `HappyStk`
	(HappyAbsSyn60  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TyEnum (Just happy_var_2) (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_126 = happySpecReduce_1  60# happyReduction_126
happyReduction_126 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn60
		 ([(happy_var_1,[],Nothing)]
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3  60# happyReduction_127
happyReduction_127 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn60
		 (((happy_var_3,[],Nothing):happy_var_1)
	)
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happyReduce 6# 61# happyReduction_128
happyReduction_128 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TySequence happy_var_3 (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_129 = happyReduce 4# 61# happyReduction_129
happyReduction_129 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TySequence happy_var_3 Nothing
	) `HappyStk` happyRest

happyReduce_130 = happyReduce 4# 62# happyReduction_130
happyReduction_130 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TyString (Just happy_var_3)
	) `HappyStk` happyRest

happyReduce_131 = happySpecReduce_1  62# happyReduction_131
happyReduction_131 _
	 =  HappyAbsSyn18
		 (TyString Nothing
	)

happyReduce_132 = happyReduce 4# 63# happyReduction_132
happyReduction_132 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TyWString (Just happy_var_3)
	) `HappyStk` happyRest

happyReduce_133 = happySpecReduce_1  63# happyReduction_133
happyReduction_133 _
	 =  HappyAbsSyn18
		 (TyWString Nothing
	)

happyReduce_134 = happySpecReduce_2  64# happyReduction_134
happyReduction_134 (HappyAbsSyn65  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn64
		 ((happy_var_1, reverse happy_var_2)
	)
happyReduction_134 _ _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_1  65# happyReduction_135
happyReduction_135 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn65
		 ([happy_var_1]
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_2  65# happyReduction_136
happyReduction_136 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn65
		 (happy_var_2:happy_var_1
	)
happyReduction_136 _ _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_3  66# happyReduction_137
happyReduction_137 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_137 _ _ _  = notHappyAtAll 

happyReduce_138 = happyReduce 4# 67# happyReduction_138
happyReduction_138 ((HappyAbsSyn38  happy_var_4) `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn68  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Attribute (reverse happy_var_4) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_139 = happySpecReduce_1  68# happyReduction_139
happyReduction_139 _
	 =  HappyAbsSyn68
		 (True
	)

happyReduce_140 = happySpecReduce_0  68# happyReduction_140
happyReduction_140  =  HappyAbsSyn68
		 (False
	)

happyReduce_141 = happySpecReduce_1  69# happyReduction_141
happyReduction_141 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn38
		 ([happy_var_1]
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3  69# happyReduction_142
happyReduction_142 (HappyAbsSyn39  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn38
		 (happy_var_3:happy_var_1
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happyReduce 5# 70# happyReduction_143
happyReduction_143 (_ `HappyStk`
	(HappyAbsSyn50  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Exception happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_144 = happySpecReduce_0  71# happyReduction_144
happyReduction_144  =  HappyAbsSyn50
		 ([]
	)

happyReduce_145 = happySpecReduce_1  71# happyReduction_145
happyReduction_145 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_145 _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_1  72# happyReduction_146
happyReduction_146 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn50
		 ([happy_var_1]
	)
happyReduction_146 _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_2  72# happyReduction_147
happyReduction_147 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_2:happy_var_1
	)
happyReduction_147 _ _  = notHappyAtAll 

happyReduce_148 = happyReduce 6# 73# happyReduction_148
happyReduction_148 ((HappyAbsSyn81  happy_var_6) `HappyStk`
	(HappyAbsSyn80  happy_var_5) `HappyStk`
	(HappyAbsSyn76  happy_var_4) `HappyStk`
	(HappyAbsSyn39  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Operation (FunId happy_var_3 Nothing happy_var_4) happy_var_2 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_149 = happySpecReduce_0  74# happyReduction_149
happyReduction_149  =  HappyAbsSyn68
		 (False
	)

happyReduce_150 = happySpecReduce_1  74# happyReduction_150
happyReduction_150 _
	 =  HappyAbsSyn68
		 (True
	)

happyReduce_151 = happySpecReduce_1  75# happyReduction_151
happyReduction_151 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_1  75# happyReduction_152
happyReduction_152 _
	 =  HappyAbsSyn18
		 (TyVoid
	)

happyReduce_153 = happySpecReduce_3  76# happyReduction_153
happyReduction_153 _
	(HappyAbsSyn76  happy_var_2)
	_
	 =  HappyAbsSyn76
		 ((reverse happy_var_2)
	)
happyReduction_153 _ _ _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_2  76# happyReduction_154
happyReduction_154 _
	_
	 =  HappyAbsSyn76
		 ([]
	)

happyReduce_155 = happySpecReduce_1  77# happyReduction_155
happyReduction_155 (HappyAbsSyn78  happy_var_1)
	 =  HappyAbsSyn76
		 ([happy_var_1]
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_3  77# happyReduction_156
happyReduction_156 (HappyAbsSyn78  happy_var_3)
	_
	(HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn76
		 (happy_var_3:happy_var_1
	)
happyReduction_156 _ _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_3  78# happyReduction_157
happyReduction_157 (HappyAbsSyn39  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn78
		 (Param happy_var_3 happy_var_2 [happy_var_1]
	)
happyReduction_157 _ _ _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_1  79# happyReduction_158
happyReduction_158 (HappyTerminal (T_mode happy_var_1))
	 =  HappyAbsSyn79
		 (Mode happy_var_1
	)
happyReduction_158 _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_0  80# happyReduction_159
happyReduction_159  =  HappyAbsSyn80
		 (Nothing
	)

happyReduce_160 = happyReduce 4# 80# happyReduction_160
happyReduction_160 (_ `HappyStk`
	(HappyAbsSyn82  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn80
		 (Just (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_161 = happySpecReduce_0  81# happyReduction_161
happyReduction_161  =  HappyAbsSyn81
		 (Nothing
	)

happyReduce_162 = happyReduce 4# 81# happyReduction_162
happyReduction_162 (_ `HappyStk`
	(HappyAbsSyn82  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (Just (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_163 = happySpecReduce_1  82# happyReduction_163
happyReduction_163 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn82
		 ([happy_var_1]
	)
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_3  82# happyReduction_164
happyReduction_164 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn82  happy_var_1)
	 =  HappyAbsSyn82
		 (happy_var_3:happy_var_1
	)
happyReduction_164 _ _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_1  83# happyReduction_165
happyReduction_165 (HappyTerminal (T_string_lit happy_var_1))
	 =  HappyAbsSyn82
		 ([happy_var_1]
	)
happyReduction_165 _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_3  83# happyReduction_166
happyReduction_166 (HappyTerminal (T_string_lit happy_var_3))
	_
	(HappyAbsSyn82  happy_var_1)
	 =  HappyAbsSyn82
		 (happy_var_3:happy_var_1
	)
happyReduction_166 _ _ _  = notHappyAtAll 

happyReduce_167 = happySpecReduce_1  84# happyReduction_167
happyReduction_167 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_167 _  = notHappyAtAll 

happyReduce_168 = happySpecReduce_1  84# happyReduction_168
happyReduction_168 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_168 _  = notHappyAtAll 

happyReduce_169 = happySpecReduce_1  84# happyReduction_169
happyReduction_169 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_169 _  = notHappyAtAll 

happyReduce_170 = happySpecReduce_1  84# happyReduction_170
happyReduction_170 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_170 _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_1  84# happyReduction_171
happyReduction_171 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (TyName happy_var_1 Nothing
	)
happyReduction_171 _  = notHappyAtAll 

happyReduce_172 = happyReduce 6# 85# happyReduction_172
happyReduction_172 (_ `HappyStk`
	(HappyAbsSyn87  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (TyFixed (Just (happy_var_3,happy_var_5))
	) `HappyStk` happyRest

happyReduce_173 = happySpecReduce_1  86# happyReduction_173
happyReduction_173 _
	 =  HappyAbsSyn18
		 (TyFixed Nothing
	)

happyReduce_174 = happySpecReduce_1  87# happyReduction_174
happyReduction_174 (HappyTerminal (T_literal happy_var_1))
	 =  HappyAbsSyn87
		 (let (IntegerLit il) = happy_var_1 in il
	)
happyReduction_174 _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_1  88# happyReduction_175
happyReduction_175 (HappyTerminal (T_string_lit happy_var_1))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_175 _  = notHappyAtAll 

happyReduce_176 = happySpecReduce_1  89# happyReduction_176
happyReduction_176 (HappyTerminal (T_id happy_var_1))
	 =  HappyAbsSyn39
		 ((Id happy_var_1)
	)
happyReduction_176 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexIDL(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	T_eof -> action 162# 162# tk (HappyState action) sts stk;
	T_semi -> cont 90#;
	T_module -> cont 91#;
	T_interface -> cont 92#;
	T_oparen -> cont 93#;
	T_cparen -> cont 94#;
	T_ocurly -> cont 95#;
	T_ccurly -> cont 96#;
	T_colon -> cont 97#;
	T_dcolon -> cont 98#;
	T_comma -> cont 99#;
	T_dot -> cont 100#;
	T_const -> cont 101#;
	T_equal -> cont 102#;
	T_eqeq -> cont 103#;
	T_neq -> cont 104#;
	T_or -> cont 105#;
	T_rel_or -> cont 106#;
	T_xor -> cont 107#;
	T_and -> cont 108#;
	T_rel_and -> cont 109#;
	T_shift happy_dollar_dollar -> cont 110#;
	T_div -> cont 111#;
	T_mod -> cont 112#;
	T_not -> cont 113#;
	T_negate -> cont 114#;
	T_question -> cont 115#;
	T_typedef -> cont 116#;
	T_type happy_dollar_dollar -> cont 117#;
	T_float happy_dollar_dollar -> cont 118#;
	T_int happy_dollar_dollar -> cont 119#;
	T_unsigned -> cont 120#;
	T_signed -> cont 121#;
	T_char -> cont 122#;
	T_wchar -> cont 123#;
	T_boolean -> cont 124#;
	T_struct -> cont 125#;
	T_union -> cont 126#;
	T_switch -> cont 127#;
	T_case -> cont 128#;
	T_default -> cont 129#;
	T_enum -> cont 130#;
	T_lt -> cont 131#;
	T_le -> cont 132#;
	T_gt -> cont 133#;
	T_ge -> cont 134#;
	T_osquare -> cont 135#;
	T_csquare -> cont 136#;
	T_void -> cont 137#;
	T_mode happy_dollar_dollar -> cont 138#;
	T_literal happy_dollar_dollar -> cont 139#;
	T_string_lit happy_dollar_dollar -> cont 140#;
	T_id happy_dollar_dollar -> cont 141#;
	T_attribute -> cont 142#;
	T_plus -> cont 143#;
	T_times -> cont 144#;
	T_minus -> cont 145#;
	T_string -> cont 146#;
	T_wstring -> cont 147#;
	T_sequence -> cont 148#;
	T_object -> cont 149#;
	T_any -> cont 150#;
	T_octet -> cont 151#;
	T_oneway -> cont 152#;
	T_fixed -> cont 153#;
	T_exception -> cont 154#;
	T_raises -> cont 155#;
	T_context -> cont 156#;
	T_readonly -> cont 157#;
	T_include_start happy_dollar_dollar -> cont 158#;
	T_include_end -> cont 159#;
	T_pragma happy_dollar_dollar -> cont 160#;
	T_unknown happy_dollar_dollar -> cont 161#;
	_ -> happyError' tk
	})

happyError_ tk = happyError' tk

happyThen :: () => LexM a -> (a -> LexM b) -> LexM b
happyThen = (thenLexM)
happyReturn :: () => a -> LexM a
happyReturn = (returnLexM)
happyThen1 = happyThen
happyReturn1 :: () => a -> LexM a
happyReturn1 = happyReturn
happyError' :: () => (IDLToken) -> LexM a
happyError' tk = (\token -> happyError) tk

parseIDL = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: LexM a
happyError = do
 l   <- getSrcLoc
 str <- getStream
 ioToLexM (ioError (userError (show l ++ ": Parse error: " ++ takeWhile (/='\n') str)))
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 1#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int# ->                    -- token number
         Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  1# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
