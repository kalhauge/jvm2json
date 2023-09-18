{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Codec (
  toJSONClass,
  toEncodingClass,
  parseJSONClass,
  prettyClass,
  debugClass,
) where

-- aeson

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as Aeson

-- base
import Data.Bifunctor (first)
import Data.String (IsString (fromString))
import Prelude hiding (all, any, null)

-- containers
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

-- conedec
import Conedec

-- jvmhs
import Jvmhs.Data.Class
import Jvmhs.Data.Code
import Jvmhs.Data.Type

-- jvm-binary
import qualified Language.JVM as B

-- unordered-containers

import Conedec.Json
import Conedec.Json.Doc
import qualified Data.HashMap.Strict as HashMap

toJSONClass :: Class -> Either String Value
toJSONClass = runWithError . toJSONViaCodec @V1 @Doc (ref @"Class")

toEncodingClass :: Class -> Either String Aeson.Encoding
toEncodingClass = runWithError . toEncodingViaCodec @V1 @Doc (ref @"Class")

parseJSONClass :: Value -> Aeson.Parser Class
parseJSONClass = parseJSONViaCodec @V1 @Doc (ref @"Class")

debugClass :: IO ()
debugClass = debugCodec @V1 (ref @"Class")

prettyClass :: Doc
prettyClass = prettyViaCodec @V1 (ref @"Class")

data V1

class (IsString ann, FromExample ValueC ann, Semigroup ann) => IsAnn ann

instance IsAnn Doc

instance IsAnn ann => Def "ClassName" ValueC V1 ann ClassName where
  def = codecClassName

codecClassName :: IsAnn ann => Codec ValueC ctx ann ClassName
codecClassName =
  codecTextSerializeable
    <?> "a name of a class, packages seperated by '/'"
      <!> "java/util/Object"

instance IsAnn ann => Def "BaseType" ValueC V1 ann JBaseType where
  def = any do
    #ifJTByte =: "byte"
    #ifJTChar =: "char"
    #ifJTDouble =: "double"
    #ifJTFloat =: "float"
    #ifJTInt =: "int"
    #ifJTLong =: "long"
    #ifJTShort =: "short"
    #ifJTBoolean =: "boolean"

instance IsAnn ann => Def "LocalType" ValueC V1 ann B.LocalType where
  def = any do
    #ifLInt =: "int"
    #ifLLong =: "long"
    #ifLFloat =: "float"
    #ifLDouble =: "double"
    #ifLRef =: "ref"

instance IsAnn ann => Def "ArithmeticType" ValueC V1 ann B.ArithmeticType where
  def = any do
    #ifMInt =: "int"
    #ifMLong =: "long"
    #ifMFloat =: "float"
    #ifMDouble =: "double"

instance IsAnn ann => Def "Parameter" ObjectC V1 ann Parameter where
  def = all do
    #_parameterNameAndFlags =: any do
      #ifNothing =: emptyObject
      #ifJust =: all do
        #fst ~ "name" <: text
        #snd
          ~ "access"
          <: codecSet
            ( any do
                #ifPFinal =: "final"
                #ifPSynthetic =: "synthetic"
                #ifPMandated =: "mandated"
            )
    #_parameterVisible ~ "visible" <: bool
    #_parameterType ~ "type" <: codecAnnotated (ref @"Type")

codecSet :: Ord a => Codec ValueC ctx ann a -> Codec ValueC ctx ann (Set.Set a)
codecSet c = bimap Set.toList Set.fromList $ manyOfList c

instance IsAnn ann => Def "TypeVariable" ObjectC V1 ann TypeVariable where
  def = all do
    #_typeVariableName ~ "name" <: simply text
    #_typeVariableBound ~ "bound" <: ref @"ClassName"

instance IsAnn ann => Def "Annotation" ValueC V1 ann Annotation where
  def = objectAll do
    #_annotationType ~ "type" <: ref @"ClassName"
    #_annotationIsRuntimeVisible ~ "is_runtime_visible" <: bool
    #_annotationValues
      ~ "values"
      <: bimap
        (fmap (first AesonKey.fromText) . HashMap.toList)
        (HashMap.fromList . fmap (first AesonKey.toText))
        (mapOf (ref @"AnnotationValue"))

instance IsAnn ann => Def "AnnotationValue" ValueC V1 ann AnnotationValue where
  def = object $ taggedInto "type" "value" do
    #ifAVByte =: "byte" // boundIntegral
    #ifAVChar =: "char" // boundIntegral
    #ifAVInt =: "int" // boundIntegral
    #ifAVLong =: "long" // boundIntegral
    #ifAVShort =: "short" // boundIntegral
    #ifAVDouble =: "double" // realFloat
    #ifAVFloat =: "float" // realFloat
    #ifAVBoolean
      =: "boolean"
      // ( boundIntegral
            <?> "0 means false and 1 means true"
         )
    #ifAVString =: "string" // byteStringUtf8
    #ifAVEnum =: "enum" // objectAll do
      given getEnumTypeName ~ "type" <: simply (ref @"SimpleType")
      given getEnumConstName ~ "name" <: text
    #ifAVClass
      =: "class"
      // simply (optional (ref @"SimpleType"))
      <?> "is normally only class"
    #ifAVAnnotation =: "annotation" // objectAll do
      #fst ~ "type" <: ref @"ClassName"
      #snd
        ~ "values"
        <: bimap
          (fmap (first AesonKey.fromText) . HashMap.toList)
          (HashMap.fromList . fmap (first AesonKey.toText))
          (mapOf (ref @"AnnotationValue"))
    #ifAVArray =: "array" // manyOfList (ref @"AnnotationValue")

codecAnnotated
  :: Def "Annotation" ValueC ctx ann Annotation
  => Codec ObjectC ctx ann a
  -> Codec ValueC ctx ann (Annotated a)
codecAnnotated ca = objectAll do
  #_annotatedContent =: ca
  #_annotatedAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")

instance IsAnn ann => Def "ThrowsType" ObjectC V1 ann ThrowsType where
  def = tagged "kind" do
    #ifThrowsClass =: "class" // ref @"ClassType"
    #ifThrowsTypeVariable =: "typevar" // ref @"TypeVariable"

instance IsAnn ann => Def "ReferenceType" ObjectC V1 ann ReferenceType where
  def = tagged "kind" do
    #ifRefClassType =: "class" // ref @"ClassType"
    #ifRefTypeVariable =: "typevar" // ref @"TypeVariable"
    #ifRefArrayType =: "array" // "type" ~: simply (codecAnnotated $ ref @"Type")

instance IsAnn ann => Def "SimpleReferenceType" ValueC V1 ann JRefType where
  def = object $ tagged "kind" do
    #ifJTClass =: "class" // "name" ~: ref @"ClassName"
    #ifJTArray =: "array" // "type" ~: ref @"SimpleType"

instance IsAnn ann => Def "TypeParameter" ObjectC V1 ann TypeParameter where
  def = all do
    #_typeParameterName ~ "name" <: simply text
    #_typeParameterClassBound
      ~ "classbound"
      <: optional (codecAnnotated $ ref @"ThrowsType")
    #_typeParameterInterfaceBound
      ~ "interfacebound"
      <: manyOfList (codecAnnotated $ ref @"ThrowsType")

instance IsAnn ann => Def "ClassType" ObjectC V1 ann ClassType where
  def = all do
    #_classTypeName ~ "name" <: text
    #_classTypeInner ~ "inner" <: optional (codecAnnotated $ ref @"ClassType")
    #_classTypeArguments ~ "args" <: manyOfList (codecAnnotated $ ref @"TypeArgument")

instance IsAnn ann => Def "TypeArgument" ObjectC V1 ann TypeArgument where
  def = tagged "kind" do
    #ifAnyTypeArg =: "any" // emptyObject
    #ifExtendedTypeArg =: "extended" // "type" ~: codecAnnotated (ref @"ReferenceType")
    #ifImplementedTypeArg =: "implemented" // "type" ~: codecAnnotated (ref @"ReferenceType")
    #ifTypeArg =: "simple" // "type" ~: object (ref @"ReferenceType")

instance IsAnn ann => Def "Type" ObjectC V1 ann Type where
  def = any do
    #ifBaseType =: "base" ~: ref @"BaseType"
    #ifReferenceType =: ref @"ReferenceType"

instance IsAnn ann => Def "SimpleType" ValueC V1 ann JType where
  def = any do
    #ifJTBase =: ref @"BaseType"
    #ifJTRef =: ref @"SimpleReferenceType"

instance IsAnn ann => Def "Class" ValueC V1 ann Class where
  def = objectAll do
    #_className' ~ "name" <: ref @"ClassName"
    #_classAccessFlags
      ~ "access"
      <: codecSet
        ( any do
            #ifCPublic =: "public"
            #ifCFinal =: "final"
            #ifCSuper =: "super"
            #ifCInterface =: "interface"
            #ifCAbstract =: "abstract"
            #ifCSynthetic =: "synthetic"
            #ifCAnnotation =: "annotation"
            #ifCEnum =: "enum"
            #ifCModule =: "module"
        )
    #_classTypeParameters ~ "typeparams" <: manyOfList (codecAnnotated $ ref @"TypeParameter")
    #_classSuper ~ "super" <: optional (codecAnnotated $ ref @"ClassType")
    #_classInterfaces ~ "interfaces" <: manyOfList (codecAnnotated $ ref @"ClassType")
    #_classFields ~ "fields" <: manyOfList (ref @"Field")
    #_classMethods ~ "methods" <: manyOfList (ref @"Method")
    #_classBootstrapMethods
      ~ "bootstrapmethods"
      <: bimap
        IntMap.toList
        IntMap.fromList
        ( manyOfList . object $ all do
            at @0 ~ "index" <: boundIntegral
            at @1 ~ "method" <: ref @"BootstrapMethod"
        )
    #_classEnclosingMethod
      ~ "enclosingmethod"
      <: optional
        ( object $ all do
            #fst ~ "class" <: ref @"ClassName"
            #snd ~ "method" <: optional (object $ ref @"MethodId")
        )
    #_classInnerClasses ~ "innerclasses" <: manyOfList (ref @"InnerClass")
    #_classAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
    #_classVersion
      ~ "version"
      <: optional
        ( arrayAll do
            at @0 <: boundIntegral
            at @1 <: boundIntegral
        )

instance IsAnn ann => Def "InnerClass" ValueC V1 ann InnerClass where
  def = objectAll do
    #_innerClass ~ "class" <: ref @"ClassName"
    #_innerOuterClass ~ "outer" <: optional (ref @"ClassName")
    #_innerClassName ~ "name" <: optional text
    #_innerAccessFlags
      ~ "access"
      <: codecSet
        ( any do
            #ifICPublic =: "public"
            #ifICPrivate =: "private"
            #ifICProtected =: "protected"
            #ifICStatic =: "static"
            #ifICFinal =: "final"
            #ifICInterface =: "interface"
            #ifICAbstract =: "abstract"
            #ifICSynthetic =: "synthetic"
            #ifICAnnotation =: "annotation"
            #ifICEnum =: "enum"
        )

instance IsAnn ann => Def "Method" ValueC V1 ann Method where
  def = objectAll do
    #_methodName ~ "name" <: text
    #_methodAccessFlags ~ "access" <: codecSet codecMAccessFlag
    #_methodTypeParameters ~ "typeparams" <: manyOfList (codecAnnotated $ ref @"TypeParameter")
    #_methodParameters ~ "params" <: manyOfList (codecAnnotated $ ref @"Parameter")
    #_methodReturnType ~ "returns" <: codecAnnotated ("type" ~: simply (optional . object $ ref @"Type"))
    #_methodCode ~ "code" <: optional (ref @"Code")
    #_methodAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
    #_methodExceptions ~ "exceptions" <: manyOfList (codecAnnotated $ ref @"ThrowsType")
    #_methodDefaultAnnotation
      ~ "default"
      <: optional (ref @"AnnotationValue")
   where
    codecMAccessFlag = any do
      #ifMPublic =: "public"
      #ifMPrivate =: "private"
      #ifMProtected =: "protected"
      #ifMStatic =: "static"
      #ifMFinal =: "final"
      #ifMSynchronized =: "synchronized"
      #ifMBridge =: "bridge"
      #ifMVarargs =: "varargs"
      #ifMNative =: "native"
      #ifMAbstract =: "abstract"
      #ifMStrictFP =: "strictFP"
      #ifMSynthetic =: "synthetic"

instance IsAnn ann => Def "Code" ValueC V1 ann Code where
  def = objectAll do
    #_codeMaxStack ~ "max_stack" <: boundIntegral
    #_codeMaxLocals ~ "max_locals" <: boundIntegral
    #_codeExceptionTable ~ "exceptions" <: manyOfList codecExceptionHandler
    #_codeStackMap ~ "stack_map" <: optional (ref @"StackMapTable")
    #_codeByteCode ~ "bytecode" <: manyOf (ref @"ByteCodeInst")
   where
    codecExceptionHandler = objectAll do
      #_ehStart ~ "start" <: boundIntegral
      #_ehEnd ~ "end" <: boundIntegral
      #_ehHandler ~ "handler" <: boundIntegral
      #_ehCatchType ~ "catchType" <: optional (ref @"ClassName")

instance IsAnn ann => Def "VerificationTypeInfo" ValueC V1 ann (VerificationTypeInfo B.High) where
  def = object $ tagged "type" do
    given ifVTTop =: "top" // emptyObject
    given ifVTInteger =: "integer" // emptyObject
    given ifVTFloat =: "float" // emptyObject
    given ifVTLong =: "long" // emptyObject
    given ifVTDouble =: "double" // emptyObject
    given ifVTNull =: "null" // emptyObject
    given ifVTUninitializedThis =: "uninitializedthis" // emptyObject
    given ifVTUninitialized
      =: "uninitialized"
      // "index"
      ~: ( boundIntegral
            <?> "referes to the bytecode instruction that initialized it"
         )
    given ifVTObject =: "object" // "ref" ~: ref @"SimpleReferenceType"

instance IsAnn ann => Def "StackMapTable" ValueC V1 ann (StackMapTable B.High) where
  def =
    simply
      ( manyOfList
          ( objectAll do
              given getDeltaOffset ~ "index" <: boundIntegral
              given getFrameType =: tagged "type" do
                #ifSameFrame =: "same" // emptyObject
                #ifSameLocals1StackItemFrame =: "same_locals_1_stack_item_frame" // "info" ~: ref @"VerificationTypeInfo"
                #ifChopFrame =: "chop_frame" // "info" ~: boundIntegral
                #ifAppendFrame =: "append_frame" // "info" ~: manyOfList (ref @"VerificationTypeInfo")
                #ifFullFrame =: "full_frame" // all do
                  #fst ~ "locals" <: simply (manyOfList $ ref @"VerificationTypeInfo")
                  #snd ~ "stack" <: simply (manyOfList $ ref @"VerificationTypeInfo")
          )
      )
      <?> "see https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4 for more info"

instance IsAnn ann => Def "CmpOpr" ValueC V1 ann B.CmpOpr where
  def = any do
    #ifCEq =: "eq"
    #ifCNe =: "ne"
    #ifCLt =: "lt"
    #ifCGe =: "ge"
    #ifCGt =: "gt"
    #ifCLe =: "le"
    #ifCIs =: "is"
    #ifCIsNot =: "isnot"

instance IsAnn ann => Def "MethodHandle" ValueC V1 ann (B.MethodHandle B.High) where
  def = object $ tagged "handletype" do
    given ifMHField =: "field" // all do
      given getMethodHandleFieldKind ~ "kind" <: any do
        given ifMHGetField =: "getfield"
        given ifMHGetStatic =: "getstatic"
        given ifMHPutField =: "putfield"
        given ifMHPutStatic =: "putstatic"
      given getMethodHandleFieldRef ~ "ref" <: ref @"AbsFieldId"
    given ifMHMethod =: "method" // taggedInto "kind" "method" do
      given ifMHInvokeVirtual =: "virtual" // ref @"RefMethodId"
      given ifMHInvokeStatic =: "static" // ref @"RefVariableMethodId"
      given ifMHInvokeSpecial =: "special" // ref @"RefVariableMethodId"
      given ifMHNewInvokeSpecial =: "new_special" // ref @"RefMethodId"
    given ifMHInterface =: "interface" // "method" ~: simply (ref @"RefMethodId")

instance IsAnn ann => Def "JArrayType" ValueC V1 ann B.ArrayType where
  def = any do
    #ifAByte =: "byte"
    #ifAChar =: "char"
    #ifAShort =: "short"
    #ifAInt =: "int"
    #ifALong =: "long"
    #ifAFloat =: "float"
    #ifADouble =: "double"
    #ifARef =: "ref"

codecInRefType
  :: Def "SimpleReferenceType" ValueC ctx ann JRefType
  => Codec ObjectC ctx ann a
  -> Codec ObjectC ctx ann (B.InRefType a)
codecInRefType c = all do
  #inRefType ~ "ref" <: ref @"SimpleReferenceType"
  #inRefTypeId =: c

codecInClass
  :: Def "ClassName" ValueC ctx ann ClassName
  => Codec ObjectC ctx ann a
  -> Codec ObjectC ctx ann (B.InClass a)
codecInClass c = all do
  #inClassName ~ "class" <: ref @"ClassName"
  #inClassId =: c

codecFieldId
  :: ( Def "SimpleType" ValueC ctx ann JType
     )
  => Codec ObjectC ctx ann B.FieldId
codecFieldId = simply $ codecNameAndType ("type" ~: codecFieldDescriptor)

instance IsAnn ann => Def "AbsFieldId" ValueC V1 ann B.AbsFieldId where
  def = object codecAbsFieldId

codecAbsFieldId
  :: ( Def "ClassName" ValueC ctx ann ClassName
     , Def "SimpleType" ValueC ctx ann JType
     )
  => Codec ObjectC ctx ann B.AbsFieldId
codecAbsFieldId = simply $ codecInClass codecFieldId

codecMethodDescriptor
  :: Def "SimpleType" ValueC ctx ann JType
  => Codec ObjectC ctx ann B.MethodDescriptor
codecMethodDescriptor = all do
  #methodDescriptorArguments ~ "args" <: manyOfList (ref @"SimpleType")
  #methodDescriptorReturnType ~ "returns" <: simply (optional (ref @"SimpleType"))

codecFieldDescriptor
  :: Def "SimpleType" ValueC ctx ann JType
  => Codec ValueC ctx ann B.FieldDescriptor
codecFieldDescriptor = simply (ref @"SimpleType")

instance IsAnn ann => Def "RefMethodId" ValueC V1 ann (B.InRefType B.MethodId) where
  def = object codecRefMethodId

instance IsAnn ann => Def "ClassMethodId" ValueC V1 ann (B.InClass B.MethodId) where
  def = object codecClassMethodId

codecRefMethodId
  :: ( Def "MethodId" ObjectC ctx ann B.MethodId
     , Def "SimpleReferenceType" ValueC ctx ann JRefType
     )
  => Codec ObjectC ctx ann (B.InRefType B.MethodId)
codecRefMethodId = codecInRefType (ref @"MethodId")

codecClassMethodId
  :: ( Def "MethodId" ObjectC ctx ann B.MethodId
     , Def "ClassName" ValueC ctx ann ClassName
     )
  => Codec ObjectC ctx ann (B.InClass B.MethodId)
codecClassMethodId = codecInClass (ref @"MethodId")

instance IsAnn ann => Def "MethodId" ObjectC V1 ann B.MethodId where
  def = codecMethodId

codecMethodId
  :: ( Def "SimpleType" ValueC ctx ann JType
     )
  => Codec ObjectC ctx ann B.MethodId
codecMethodId = simply $ codecNameAndType codecMethodDescriptor

instance IsAnn ann => Def "RefVariableMethodId" ValueC V1 ann B.AbsVariableMethodId where
  def = object codecAbsVariableMethodId

codecAbsVariableMethodId
  :: ( Def "MethodId" ObjectC ctx ann B.MethodId
     , Def "SimpleReferenceType" ValueC ctx ann JRefType
     )
  => Codec ObjectC ctx ann B.AbsVariableMethodId
codecAbsVariableMethodId = all do
  #variableIsInterface ~ "is_interface" <: bool
  #variableMethodId =: codecRefMethodId

codecNameAndType
  :: Codec ObjectC ctx ann a
  -> Codec ObjectC ctx ann (B.NameAndType a)
codecNameAndType cdc = bimap (\(B.NameAndType t a) -> (t, a)) (uncurry B.NameAndType) $ all do
  #fst ~ "name" <: text
  #snd =: cdc

data BC = BC String [String] [String]

bc :: (Semigroup a, IsString a) => BC -> a
bc (BC name before after) = do
  "{" <> fromString name <> "} " <> fromString (show before) <> " -> " <> fromString (show after)

instance IsAnn ann => Def "ByteCodeInst" ValueC V1 ann ByteCodeInst where
  def = objectAll do
    #offset <: boundIntegral
    #opcode =: tagged "opr" do
      given ifArrayStore
        =: ( "array_store" // do
              "type" ~: ref @"JArrayType"
           )
        <?> "load a $value of $type from an $arrayref array at index $index"
        <?> bc (BC "aastore" ["arrayref", "index"] ["value"])

      given ifArrayLoad
        =: ( "array_load" // do
              "type" ~: ref @"JArrayType"
           )
        <?> "store a $value of $type in a $arrayref array at index $index"
        <?> bc (BC "aaload" ["arrayref", "index", "value"] [])

      given ifPush
        =: ( "push" // do
              "value" ~: optional (ref @"Value")
           )
        <?> "push a $value or null onto the stack"
        <?> bc (BC "*" [] ["value"])

      given ifLoad
        =: ( "load" // all do
              #fst ~ "type" <: ref @"LocalType"
              #snd ~ "index" <: boundIntegral
           )
        <?> "load a local variable $index of $type"
        <?> bc (BC "*" [] ["value"])

      given ifStore
        =: ( "store" // all do
              #fst ~ "type" <: ref @"LocalType"
              #snd ~ "index" <: boundIntegral
           )
        <?> "store a local variable $index of $type"
        <?> bc (BC "*" ["value"] [])

      given ifIncrLocal
        =: ( "incr" // all do
              #fst ~ "index" <: boundIntegral
              #snd ~ "amount" <: boundIntegral
           )
        <?> "increment local in $index by $anount"
        <?> bc (BC "iinc" [] [])

      given ifBinaryOpr
        =: ( "binary" // all do
              #snd ~ "type" <: ref @"ArithmeticType"
              #fst ~ "operant" <: any do
                given ifAdd =: "add"
                given ifSub =: "sub"
                given ifMul =: "mul"
                given ifDiv =: "div"
                given ifRem =: "rem"
           )
        <?> "does a binary operation over type $type, with $operant on $value1 $value2"
        <?> bc (BC "*" ["value1", "value2"] ["result"])

      given ifNeg
        =: ( "negate" // do
              "type" ~: ref @"ArithmeticType"
           )
        <?> "negate the $value of $type"
        <?> bc (BC "*" ["value"] ["result"])

      given ifBitOpr
        =: ( "bitopr" // all do
              #snd ~ "type" <: any do
                given ifOne =: "int"
                given ifTwo =: "long"
              #fst ~ "operant" <: any do
                given ifShL =: "shl"
                given ifShR =: "shr"
                given ifUShR =: "ushr"
                given ifAnd =: "and"
                given ifOr =: "or"
                given ifXOr =: "xor"
           )
        <?> "negate the $value of $type"
        <?> bc (BC "*" ["value"] ["result"])

      given ifCast
        =: ( "cast" // any do
              given ifCastDown
                =: untup
                  ( all do
                      #fst ~ "from" <: "int"
                      #snd ~ "to" <: any do
                        #ifMByte =: "byte"
                        #ifMChar =: "char"
                        #ifMShort =: "short"
                  )
              given ifCastTo =: all do
                #fst ~ "from" <: ref @"ArithmeticType"
                #snd ~ "to" <: ref @"ArithmeticType"
           )
        <?> "cast from one type to another, only int can be cast to a small type"

      given ifCompareLongs
        =: ("comparelongs" // emptyObject)
        <?> "compare two longs, return 1 if $value1 is larger than $value2 0 if equal and -1 if smaller in $result"
        <?> bc (BC "lcmp" ["value1", "value1"] ["result"])

      given ifCompareFloating
        =: ( "comparefloating" // all do
              #fst ~ "onnan" <: any do
                given ifTrue =: exact (Number 1)
                given ifFalse =: exact (Number (-1))
              #snd ~ "type" <: any do
                given ifOne =: "float"
                given ifTwo =: "double"
           )
        <?> "compare two floats, return 1 if $value1 is larger than $value2 0 if equal and -1 if smaller in $result"
        <?> "pushes $onnan$ if any is NaN"
        <?> bc (BC "*cmp" ["value1", "value1"] ["result"])

      given ifIf
        =: ( "if" // all do
              #fst ~ "condition" <: ref @"CmpOpr"
              #snd ~ "target" <: boundIntegral
           )
        <?> "jump to $target if the $condition over intergers is true, compare with two elements or against zero if $with_zero"
        <?> bc (BC "if_i*" ["value1 :int", "value2 : int"] [])
        <?> ("if $condition is 'is'" <> bc (BC "if_acmpeq" ["value1 : ref", "value2 : ref"] []))
        <?> ("if $condition is 'nis'" <> bc (BC "if_acmpne" ["value1 :ref ", "value2 : ref"] []))

      given ifIfZ
        =: ( "ifz" // all do
              #fst ~ "condition" <: ref @"CmpOpr"
              #snd ~ "target" <: boundIntegral
           )
        <?> "jump to $target if the $condition is true, compare with one element against null or 0"
        <?> bc (BC "if*" ["value1:int"] [])
        <?> bc (BC "ifnull" ["objectref"] [])
        <?> bc (BC "ifnonnull" ["objectref"] [])

      given ifGoto
        =: ( "goto"
              // ("target" ~: boundIntegral)
           )
        <?> "jump to $target"
        <?> bc (BC "goto*" [] [])

      given ifJsr
        =: ("jsr" // ("target" ~: boundIntegral))
        <?> "jump to $target an add an $address on stack, normaly not used"
        <?> bc (BC "jsr*" [] ["address"])

      given ifRet
        =: ( "ret"
              // ("local" ~: boundIntegral)
           )
        <?> "returns the execution to adress in $local - not return"
        <?> bc (BC "ret" [] [])

      given ifTableSwitch
        =: ( "tableswitch" // all do
              #fst ~ "default" <: boundIntegral
              #snd =: all do
                given getSwitchLow
                  ~ "low"
                  <: boundIntegral
                  <?> "value to substract from index before looking up"
                given getSwitchOffsets
                  ~ "targets"
                  <: manyOf boundIntegral
           )
        <?> "jump to $index - $low in $targets, otherwise jump to $default"
        <?> bc (BC "tableswitch" ["index"] [])

      given ifLookupSwitch
        =: ( "lookupswitch" // all do
              #fst ~ "default" <: boundIntegral
              #snd
                ~ "targets"
                <: manyOf
                  ( object $ all do
                      #fst ~ "key" <: boundIntegral
                      #snd ~ "target" <: boundIntegral
                  )
           )
        <?> "jump to $key = $index in $targets, otherwise jump to $default"
        <?> bc (BC "lookupswitch" ["index"] [])

      given ifGet
        =: ( "get" // all do
              #fst
                ~ "static"
                <: bimap
                  ( \case
                      B.FldStatic -> True
                      B.FldField -> False
                  )
                  ( \case
                      True -> B.FldStatic
                      False -> B.FldField
                  )
                  bool
              #snd ~ "field" <: object codecAbsFieldId
           )
        <?> "get value from $field (might be $static)"
        <?> bc (BC "getfield" ["objectref"] ["value"])
        <?> ("if static " <> bc (BC "getstatic" [] ["value"]))
      given ifPut
        =: ( "put" // all do
              #fst
                ~ "static"
                <: bimap
                  ( \case
                      B.FldStatic -> True
                      B.FldField -> False
                  )
                  ( \case
                      True -> B.FldStatic
                      False -> B.FldField
                  )
                  bool
              #snd ~ "field" <: object codecAbsFieldId
           )
        <?> "put a value into a $field (might be $static)"
        <?> bc (BC "putfield" ["objectref", "value"] [])
        <?> ("if static " <> bc (BC "putstatic" ["value"] []))
      given ifInvoke
        =: ( "invoke"
              // tagged "access" do
                given ifInvkSpecial
                  =: ("special" // "method" ~: ref @"RefVariableMethodId")
                given ifInvkVirtual
                  =: ("virtual" // "method" ~: ref @"RefMethodId")
                given ifInvkStatic
                  =: ("static" // "method" ~: ref @"RefVariableMethodId")
                given ifInvkInterface
                  =: ( "interface" // all do
                        #fst ~ "stack_size" <: boundIntegral
                        #snd ~ "method" <: simply (ref @"RefMethodId")
                     )
                given ifInvkDynamic
                  =: ( "dynamic" // all do
                        given getInvokeDynamicAttrIndex ~ "index" <: boundIntegral
                        given getInvokeDynamicMethod ~ "method" <: object (ref @"MethodId")
                     )
           )
        <?> "invoke a method, consult the documentation"
        <?> bc (BC "invokevirtual" ["objectref", "arg1", "arg2", "..."] ["result"])
      given ifNew
        =: ( "new"
              // ("class" ~: ref @"ClassName")
           )
        <?> "create a new $object of $class"
        <?> bc (BC "new" [] ["objectref"])
      given ifNewArray
        =: ( "newarray"
              // bimap
                (\(B.NewArrayType a b) -> (a, b))
                (uncurry B.NewArrayType)
                ( all do
                    #fst ~ "dim" <: boundIntegral
                    #snd ~ "type" <: ref @"SimpleType"
                )
              <?> "create a $dim - dimentional array of size $count and $type"
              <?> bc (BC "newarray" ["count1", "count2", "..."] ["objectref"])
           )
      given ifArrayLength
        =: ( "arraylength" // emptyObject
              <?> "finds the length of an array"
              <?> bc (BC "arraylength" ["array"] ["length"])
           )
      given ifThrow
        =: ( "throw" // emptyObject
              <?> "throws an exception"
              <?> bc (BC "athrow" ["objectref"] ["objectref"])
           )
      given ifCheckCast
        =: ( "checkcast" // ("type" ~: ref @"SimpleReferenceType")
              <?> "throws a ClassCastException if $objectref can be cast to type $type"
              <?> bc (BC "checkcast" ["objectref"] ["objectref"])
           )
      given ifInstanceOf
        =: ( "instanceof" // ("type" ~: ref @"SimpleReferenceType")
              <?> "returns 1 if $objectref can be cast to type $type, otherwise 0"
              <?> bc (BC "checkcast" ["objectref"] ["result"])
           )
      given ifMonitor
        =: ( "monitor"
              // ("enter" ~: bool)
              <?> "enter or exit a monitor (according to $enter)"
              <?> bc (BC "monitorenter" ["monitor"] [])
              <?> bc (BC "monitorexit" ["monitor"] [])
           )
      given ifReturn
        =: ( "return"
              // ("type" ~: optional (ref @"LocalType"))
           )
        <?> "return a $value of $type"
        <?> bc (BC "*return" ["value"] [])
      given ifNop =: ("nop" // emptyObject <?> "No operation")
      given ifPop
        =: ( "pop"
              // ( "words"
                    ~: any do
                      given ifOne =: exact (Number 1)
                      given ifTwo =: exact (Number 2)
                 )
              <?> "pop $words words elements from the stack, "
              <?> bc (BC "pop1" ["value1"] [])
              <?> bc (BC "pop2" ["value1", "value2"] [])
              <?> bc (BC "pop2" ["value1:long"] [])
           )
      given ifDup
        =: ( "dup"
              // ( "words" ~: any do
                    given ifOne =: exact (Number 1)
                    given ifTwo =: exact (Number 2)
                 )
              <?> "duplicate $words from stack"
              <?> bc (BC "dup" ["value1"] ["value1", "value1"])
              <?> bc (BC "dup2" ["v1", "v2"] ["v1", "v2", "v1", "v2"])
              <?> bc (BC "dup2" ["value1:long"] ["value1:long", "value1:long"])
           )
      given ifDupX1
        =: ( "dup_x1"
              // ( "words" ~: any do
                    given ifOne =: exact (Number 1)
                    given ifTwo =: exact (Number 2)
                 )
              <?> "duplicate $words on the stack insert one word deep, "
              <?> bc (BC "dup_x1" ["v2", "v1"] ["v1", "v2", "v1"])
              <?> bc
                ( BC
                    "dup2_x1"
                    ["v3", "v2", "v1"]
                    ["v2", "v1", "v3", "v2", "v1"]
                )
              <?> bc
                ( BC
                    "dup2_x1"
                    ["v2", "value1:long"]
                    ["value1:long", "v2", "value1:long"]
                )
           )
      given ifDupX2
        =: ( "dup_x2"
              // ( "words" ~: any do
                    given ifOne =: exact (Number 1)
                    given ifTwo =: exact (Number 2)
                 )
              <?> "duplicate $words from the stack insert 2 words deep"
              <?> bc (BC "dup_x2" ["v3", "v2", "v1"] ["v1", "v3", "v2", "v1"])
           )
      given ifSwap
        =: ( "swap" // emptyObject
              <?> "swap two elements on the stack"
              <?> bc (BC "swap" ["v2", "v1"] ["v1", "v2"])
           )

instance IsAnn ann => Def "Field" ValueC V1 ann Field where
  def = objectAll do
    #_fieldName ~ "name" <: text
    #_fieldAccessFlags ~ "access" <: codecSet codecFAccessFlag
    #_fieldType ~ "type" <: codecAnnotated (ref @"Type")
    #_fieldValue ~ "value" <: optional (ref @"Value")
    #_fieldAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
   where
    codecFAccessFlag = any do
      #ifFPublic =: "public"
      #ifFPrivate =: "private"
      #ifFProtected =: "protected"
      #ifFStatic =: "static"
      #ifFFinal =: "final"
      #ifFVolatile =: "volatile"
      #ifFTransient =: "transient"
      #ifFSynthetic =: "synthetic"
      #ifFEnum =: "enum"

instance IsAnn ann => Def "Value" ValueC V1 ann JValue where
  def = object $ tagged "type" do
    #ifVInteger =: "integer" // ("value" ~: boundIntegral)
    #ifVLong =: "long" // ("value" ~: boundIntegral)
    #ifVFloat =: "float" // ("value" ~: realFloat)
    #ifVDouble =: "double" // ("value" ~: realFloat)
    #ifVString =: "string" // ("value" ~: byteStringUtf8)
    #ifVClass =: "class" // ("value" ~: ref @"SimpleReferenceType")
    #ifVMethodType =: "methodtype" // ("value" ~: object codecMethodDescriptor)
    #ifVMethodHandle =: "methodhandle" // ("value" ~: ref @"MethodHandle")

instance IsAnn ann => Def "BootstrapMethod" ValueC V1 ann BootstrapMethod where
  def = object $ all do
    #_bootstrapMethodHandle ~ "handle" <: ref @"MethodHandle"
    #_bootstrapMethodArguments ~ "args" <: manyOfList (ref @"Value")

codecTextSerializeable :: B.TextSerializable x => Codec ValueC ctx ann x
codecTextSerializeable = dimap (pure . B.serialize) B.deserialize text
