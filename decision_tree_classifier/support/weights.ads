
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with ML_Types;

package Weights is

   type Weight_Type is (No_Weight, Balanced_Weight, Weight_Dict, Weights_List);
--     type Weight_Data is record
--        Label  : Classifier_Types.Class_Label := To_Unbounded_String ("None");
--        Weight : Float := 1.0;
--     end record;

   package Weight_Dictionary is new Ada.Containers.Ordered_Maps
     (Classifier_Types.Class_Label, Float);
   subtype Weight_Map is Weight_Dictionary.Map;

--     package Weight_Package is new Ada.Containers.Vectors
--       (Positive, Float);
--     subtype Weight_List is Weight_Package.Vector;

   use Float_Package;
   package Weight_Lists_Package is new Ada.Containers.Vectors
     (Positive, Weight_List);
   subtype Weight_Lists_List is Weight_Lists_Package.Vector;

   Weights_Error : exception;

   function Compute_Class_Weights (Weight_Kind   : Weight_Type;
                                   Class_Weights : Weight_List;
                                   Classes       : ML_Types.Value_Data_List;
                                   Y             : ML_Types.Value_Data_List)
                                   return Weight_List;
   function Compute_Sample_Weight (Weight_Kind   : Weight_Type;
                                   Y             : ML_Types.Value_Data_List;
                                   Num_Outputs   : Integer;
                                   Class_Weights : Weight_List :=
                                     Float_Package.Empty_Vector;
                                   Indices        : Integer_List :=
                                     Integer_Package.Empty_Vector)
                                   return Weight_List;

end Weights;
