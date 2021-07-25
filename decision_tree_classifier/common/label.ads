
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Utilities; use Classifier_Utilities;

package Label is

   subtype Param_Label is Unbounded_String;
   package Params_Dictionary is new Ada.Containers.Ordered_Maps
     (Param_Label, Float);
   subtype Params_Map is Params_Dictionary.Map;

   type Label_Encoder is record
      Classes : Integer_List;
   end record;

   function Fit_Transform (Self : in out Label_Encoder;
                           Y    : Integer_List := Integer_Package.Empty_Vector)
                           return Integer_List;

end Label;
