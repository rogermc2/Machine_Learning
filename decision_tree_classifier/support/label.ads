
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with ML_Types;
with Estimator;

package Label is

   subtype Param_Label is Unbounded_String;
   package Params_Dictionary is new Ada.Containers.Ordered_Maps
     (Param_Label, Float);
   subtype Params_Map is Params_Dictionary.Map;

   --  Label_Encoder should be used to encode target values, i.e. Y and
   --  not the input X.
   type Label_Encoder is record
      --  Estimator_Kind declared in base.py class ClassifierMixin
      Estimator_Kind : Estimator.Estimator_Type :=
                         Estimator.Classifier_Estimator;
      Classes        : ML_Types.Value_Data_List;
   end record;

   Label_Error : Exception;

   procedure Fit (Encoder : in out Label_Encoder; Y : ML_Types.Value_Data_List);
   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : ML_Types.Value_Data_List)
                           return Natural_List;
   function Inverse_Transform  (Self : in out Label_Encoder;
                               Y    : Natural_List)
                               return ML_Types.Value_Data_List;
   function Transform (Y    : ML_Types.Value_Data_List)
                       return Natural_List;

end Label;
