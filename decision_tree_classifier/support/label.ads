
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with ML_Types;

package Label is

   subtype Param_Label is Unbounded_String;
   package Params_Dictionary is new Ada.Containers.Ordered_Maps
     (Param_Label, Float);
   subtype Params_Map is Params_Dictionary.Map;

   type Label_Encoder is record
      Classes : Integer_List;
   end record;

   function Fit (Y : ML_Types.Value_Data_List) return Label_Encoder;
   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : ML_Types.Value_Data_List)
                           return Integer_List;
--     function Inverse_Transform (Self : in out Label_Encoder;
--                                 Y    : ML_Types.Value_Data_List)
--                                 return Integer_List;
   function Transform (Self : in out Label_Encoder;
                       Y    : ML_Types.Value_Data_List)
                       return Integer_List;

end Label;
