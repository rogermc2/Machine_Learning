
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;
with Estimator;

package Tree_Label is

   type Class_Type is (Class_Unique, Class_Label);
   subtype Param_Label is Unbounded_String;

   package Params_Dictionary is new Ada.Containers.Ordered_Maps
     (Param_Label, Float);
   subtype Params_Map is Params_Dictionary.Map;

   --  Label_Encoder should be used to encode target values,
   --  i.e. Y and not the input X.
   type Label_Encoder (Encoder_Kind : Class_Type) is record
      --  Estimator_Kind declared in base.py class ClassifierMixin
      Estimator_Kind : Estimator.Estimator_Type :=
                         Estimator.Classifier_Estimator;
      case Encoder_Kind is
         when Class_Unique => Uniques : ML_Types.Value_Data_List;
         when Class_Label => Classes  : NL_Types.Natural_List;
      end case;
   end record;

   Label_Error : Exception;

   procedure Fit (Encoder : in out Label_Encoder;
                  Y       : ML_Types.Value_Data_List);
   function Fit_Transform (Encoder : in out Label_Encoder;
                           Y    : ML_Types.Value_Data_List)
                            return NL_Types.Natural_List;
   function Inverse_Transform  (Self : in out Label_Encoder;
                                Labels : NL_Types.Natural_List)
                                 return ML_Types.Value_Data_List;
   function Transform (Self : in out Label_Encoder;
                       Y    : ML_Types.Value_Data_List)
                        return NL_Types.Natural_List;

end Tree_Label;