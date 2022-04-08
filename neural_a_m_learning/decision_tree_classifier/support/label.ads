
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Estimator;

package Label is

    type Class_Type is (Class_Unique, Class_Label);
    subtype Param_Label is Unbounded_String;

    package Params_Dictionary is new Ada.Containers.Ordered_Maps
      (Param_Label, Float);
    subtype Params_Map is Params_Dictionary.Map;

    --  Label_Encoder should be used to encode target values,
    --  i.e. Y and not the input X.
    type Label_Encoder (Encoder_Kind : Class_Type; Num_Samples : Positive) is
       record
       --  Estimator_Kind declared in base.py class ClassifierMixin
           Estimator_Kind : Estimator.Estimator_Type :=
                              Estimator.Classifier_Estimator;
           case Encoder_Kind is
           when Class_Unique => Uniques : Integer_Array (1 .. Num_Samples);
           when Class_Label => Classes  : Natural_Array (1 .. Num_Samples);
           end case;
       end record;

    Label_Error : Exception;

    procedure Fit (Encoder : in out Label_Encoder; Y : Integer_Array);
    function Fit_Transform (Encoder : in out Label_Encoder;
                            Y       : Integer_Array) return Natural_Array;
    function Inverse_Transform (Self   : in out Label_Encoder;
                                Labels : Natural_Array) return Integer_Array;
    function Inverse_Transform (Self : in out Label_Encoder; Y : Integer_Array)
                               return Integer_Array;
    function Inverse_Transform (Self : in out Label_Encoder; Y : Integer_Matrix)
                               return Integer_Matrix;
    function Transform (Self : in out Label_Encoder; Y : Integer_Array)
                       return Natural_Array;

end Label;
