--  Adapted from scikit-learn/scikit-learn.git sklearn/preprocessing/_label.py

with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Estimator;
with Multiclass_Utils;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types; use NL_Types;

package Label is

    type Class_Type is (Class_Unique, Class_Label, Class_Label_List);
    subtype Param_Label is Unbounded_String;

    package Params_Dictionary is new Ada.Containers.Ordered_Maps
      (Param_Label, Float);
    subtype Params_Map is Params_Dictionary.Map;

    type Label_Binarizer
      (Y_Kind : Multiclass_Utils.Y_Type := Multiclass_Utils.Y_Unknown) is record
        Neg_Label   : Float := 0.0;
        Pos_Label   : Float := 1.0;
        Classes     : Integer_List;
    end record;

    type Multi_Label_Binarizer is record
        Classes : Integer_List;
    end record;

    type UB_Label_Binarizer
      (Y_Kind : Multiclass_Utils.Y_Type := Multiclass_Utils.Y_Unknown) is record
        Classes : Unbounded_List;
    end record;

    --  Label_Encoder should be used to encode target values,
    --  i.e. Y and not the input X.
    type Label_Encoder (Encoder_Kind : Class_Type; Num_Items : Positive) is
       record
       --  Estimator_Kind declared in base.py class ClassifierMixin
           Estimator_Kind : Estimator.Estimator_Type :=
                              Estimator.Classifier_Estimator;
           case Encoder_Kind is
               when Class_Unique => Uniques          : Integer_Array (1 .. Num_Items);
               when Class_Label => Classes           : Natural_Array (1 .. Num_Items);
               when Class_Label_List => Classes_List : Integer_List;
           end case;
       end record;

    Label_Error : Exception;

    procedure C_Init (LB        : in out Label_Binarizer;
                      Neg_Label : Float := 0.0; Pos_Label : Float := 1.0);
    procedure C_Init (MLB     : in out Multi_Label_Binarizer;
                      Classes : Integer_List := Integer_Package.Empty_Vector);
    procedure Fit (Binarizer : in out Label_Binarizer; Classes : Integer_List);
    procedure Fit (Binarizer : in out Label_Binarizer; Y : Binary_Matrix);
    procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Array);
    procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Matrix);
    procedure Fit (Binarizer : in out Multi_Label_Binarizer;
                   Y : Integer_Matrix);
    procedure Fit (Binarizer : in out Label_Binarizer;
                   Y         : Array_Of_Integer_Lists);
    procedure Fit (Binarizer : in out UB_Label_Binarizer;
                   Y : Unbounded_String_Array);
    procedure Fit (Binarizer : in out UB_Label_Binarizer;
                   Y : Unbounded_String_Matrix);
    procedure Fit (Encoder : in out Label_Encoder; Y : Integer_Array);
    function Fit_Transform (Binarizer : in out Label_Binarizer;
                            Y         : Integer_Matrix) return Binary_Matrix;
    function Fit_Transform
      (Binarizer : in out UB_Label_Binarizer; Y : Unbounded_String_Array)
       return Binary_Matrix;
    function Fit_Transform
      (Binarizer : in out UB_Label_Binarizer; Y : Unbounded_String_Matrix)
       return Binary_Matrix;
    function Fit_Transform (Encoder : in out Label_Encoder;
                            Y       : Integer_Array) return Natural_Array;
    --      function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix)
    --                                  return Real_Float_Matrix;
    --      function Inverse_Transform (Self : Label_Binarizer; Y : Boolean_Matrix)
    --                                  return Integer_Matrix;
    function Inverse_Transform (Self : Label_Binarizer; Y : Real_Float_Matrix)
                                return Integer_Matrix;
    function Inverse_Transform (Self : UB_Label_Binarizer; Y : Binary_Matrix)
                                return Unbounded_String_Matrix;
    function Inverse_Transform (Self   : Label_Encoder;
                                Labels : Natural_Array) return Integer_Array;
    function Inverse_Transform (Self : Label_Encoder; Y : Integer_Array)
                                return Integer_Array;
    function Inverse_Transform (Self : Label_Encoder; Y : Integer_Matrix)
                                return Integer_Matrix;
    function Label_Binarize (Y, Classes : Integer_List;
                             Neg_Label  : Integer := 0) return Boolean_Matrix;
    function Label_Binarize (Y         : Integer_Matrix; Classes : Integer_List;
                             Neg_Label : Integer := 0) return Binary_Matrix;
    function Transform (Self : Label_Binarizer; Y : Binary_Matrix)
                        return Binary_Matrix;
    function Transform (Self : Label_Binarizer; Y : Integer_Matrix)
                        return Binary_Matrix;
    function Transform (Self : Label_Binarizer; Y : Integer_List)
                        return Boolean_Matrix;
    function Transform (Self : Label_Binarizer; Y : Array_Of_Integer_Lists)
                        return Boolean_Matrix;
    function Transform (Self : Label_Binarizer; Y : Array_Of_Integer_Lists)
                        return Binary_Matrix;
    function Transform (Self : UB_Label_Binarizer; Y : Unbounded_String_Array)
                        return Binary_Matrix;
    function Transform (Self : UB_Label_Binarizer; Y : Unbounded_String_Matrix)
                        return Binary_Matrix;
    function Transform (Self : Label_Encoder; Y : Integer_Array)
                        return Natural_Array;

end Label;
