
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with ML_Types;

package Weights is

   type Weight_Type is (No_Weight, Balanced_Weight, Weight_Dict, List_Of_Weights);
   type Weight_Data is record
      Label  : Classifier_Types.Class_Label := To_Unbounded_String ("None");
      Weight : Float := 1.0;
   end record;

   package Weight_Dictionary is new Ada.Containers.Ordered_Maps
     (Classifier_Types.Class_Label, Float);
   subtype Weight_Map is Weight_Dictionary.Map;

   package Weight_Package is new Ada.Containers.Vectors
     (Positive, Weight_Data);
   subtype Weight_List is Weight_Package.Vector;

   use Weight_Package;
   package Weight_Lists_Package is new Ada.Containers.Vectors
     (Positive, Weight_List);
   subtype Weight_Lists_List is Weight_Lists_Package.Vector;

   Weights_Error : exception;

   function Compute_Sample_Weight (Weight_Kind   : Weight_Type;
                                   Y              : ML_Types.List_Of_Value_Data_Lists;
                                   Class_Weights  : Weight_List :=
                                     Weight_Package.Empty_Vector;
                                   Indices        : Integer_List :=
                                     Integer_Package.Empty_Vector)
                                   return Weight_List;

end Weights;
