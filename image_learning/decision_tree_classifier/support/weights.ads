
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with ML_Types;

package Weights is

    type Weight_Type is (No_Weight, Balanced_Weight, Weight_Dict, Weights_List);

    package Weight_Dictionary is new Ada.Containers.Ordered_Maps
      (Classifier_Types.Class_Label, Float);
    subtype Weight_Map is Weight_Dictionary.Map;

    use Float_Package;
    subtype Weight_List is Float_Package.Vector;

    package Weight_Lists_2D_Package is new Ada.Containers.Vectors
      (Positive, Weight_List);
    subtype Weight_Lists_2D is Weight_Lists_2D_Package.Vector;

    use Weight_Lists_2D_Package;
    package Weight_Lists_3D_Package is new Ada.Containers.Vectors
      (Positive, Weight_Lists_2D);
    subtype Weight_Lists_3D is Weight_Lists_3D_Package.Vector;
    subtype Weight_Lists_3D_Cursor is Weight_Lists_3D_Package.Cursor;

    Weights_Error : exception;

    function Compute_Class_Weights (Weight_Kind   : Weight_Type;
                                    Class_Weights : Weight_List;
                                    Classes       : ML_Types.Value_Data_List;
                                    Y             : ML_Types.Value_Data_List)
                                    return Weight_List;
    function Compute_Sample_Weight
      (Weight_Kind    : Weight_Type;
       Y              : ML_Types.Value_Data_Lists_2D;
       Class_Weights  : Weight_List := Float_Package.Empty_Vector;
       Indices        : Integer_List := Integer_Package.Empty_Vector)
       return Weight_List;
    function Get_Column (Weights  : Weight_Lists_2D; Data_Index : Positive)
                         return Weight_List;
    function Get_Column (Weights  : Weight_Lists_3D; Data_Index : Positive)
                         return Weight_Lists_2D;
    function Max (Weights : Weight_List) return Positive;
    function Max (Weights : Weight_List) return Float;
    function Max (Weights_3D : Weight_Lists_3D) return Float;
    function Min (Weights : Weight_List) return Positive;
    function Min (Weights : Weight_List) return Float;
    function Min (Weights_3D : Weight_Lists_3D) return Float;
    function Transpose (Weights : Weight_Lists_2D) return Weight_Lists_2D;

end Weights;
