
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Multiclass_Utils;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Estimator;

package Label is

   type Class_Type is (Class_Unique, Class_Label);
   subtype Param_Label is Unbounded_String;

   package Params_Dictionary is new Ada.Containers.Ordered_Maps
     (Param_Label, Float);
   subtype Params_Map is Params_Dictionary.Map;

   type Label_Binarizer is record
      Neg_Label : Integer := 0;
      Pos_Label : Integer := 1;
      Classes   : NL_Types.Integer_List;
      Y_Kind    : Multiclass_Utils.Y_Type := Multiclass_Utils.Y_Unknown;
   end record;

   --  Label_Encoder should be used to encode target values,
   --  i.e. Y and not the input X.
   type Label_Encoder (Encoder_Kind : Class_Type; Num_Items : Positive) is
      record
         --  Estimator_Kind declared in base.py class ClassifierMixin
         Estimator_Kind : Estimator.Estimator_Type :=
                            Estimator.Classifier_Estimator;
         case Encoder_Kind is
            when Class_Unique => Uniques : Integer_Array (1 .. Num_Items);
            when Class_Label => Classes  : Natural_Array (1 .. Num_Items);
         end case;
      end record;

   Label_Error : Exception;

   procedure Fit (Binarizer : in out Label_Binarizer; Y : Integer_Matrix);
   procedure Fit (Encoder : in out Label_Encoder; Y : Integer_Array);
   function Fit_Transform (Encoder : in out Label_Encoder;
                           Y       : Integer_Array) return Natural_Array;
   function Inverse_Transform (Self   : Label_Encoder;
                               Labels : Natural_Array) return Integer_Array;
   function Inverse_Transform (Self : Label_Encoder; Y : Integer_Array)
                                return Integer_Array;
   function Inverse_Transform (Self : Label_Encoder; Y : Integer_Matrix)
                                return Integer_Matrix;
   function Transform (Self : Label_Binarizer; Y : Integer_Array)
                        return Boolean_Array;
   function Transform (Self : Label_Encoder; Y : Integer_Array)
                        return Natural_Array;

end Label;
