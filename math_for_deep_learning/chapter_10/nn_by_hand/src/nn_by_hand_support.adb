
--  with Maths;

with Load_Dataset;
with ML_Types;
with NL_Types;

package body NN_By_Hand_Support is

   function Means (M : Real_Float_Matrix) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   procedure Build_Dataset (X_Train, Y_Train,
                            X_Test, Y_Test : out Real_Float_Vector) is
      --        use maths.Float_Math_Functions;
      Iris_Data        : constant Load_Dataset.Iris_Data_Record :=
                           Load_Dataset.Load_Iris ("src/iris.csv");
      Features         : constant NL_Types.Float_List_2D := Iris_Data.Features;
      Target           : constant ML_Types.Integer_List := Iris_Data.Target;
      Num_Features     : constant Positive := Iris_Data.Num_Features;
      Feature_Row      : NL_Types.Float_List;
      X                : Real_Float_Matrix
        (Features.First_Index .. Features.Last_Index, 1 ..2);
   begin
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := Feature_Row (1);
         X (row, 2) := Feature_Row (2);
      end loop;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   function Means (M : Real_Float_Matrix) return Real_Float_Vector is
      M_Length : constant Float := Float (M'Length);
      Sum1     : Float := 0.0;
      Sum2     : Float := 0.0;
      Result   : Real_Float_Vector (1 .. 2);
   begin
      for row in M'Range loop
         Sum1 := Sum1 + M (row, 1);
         Sum2 := Sum2 + M (row, 2);
      end loop;

      for row in M'Range loop
         Result (1) := Sum1 / M_Length;
         Result (2) := Sum2 / M_Length;
      end loop;

      return Result;

   end Means;

   --  -------------------------------------------------------------------------

   function Standard_Deviation (M : Real_Float_Matrix)
                                return Real_Float_Vector is
      M_Length  : constant Float := Float (M'Length);
      Mean_Vals : constant Real_Float_Vector := Means (M);
      Diffs     : Real_Float_Matrix (M'Range, M'Range (2));
      Sum1      : Float := 0.0;
      Sum2      : Float := 0.0;
      Result    : Real_Float_Vector (1 .. 2);
   begin
      for row in M'Range loop
         Sum1 := Sum1 + M (row, 1);
         Sum2 := Sum2 + M (row, 2);
      end loop;

      for row in M'Range loop
         Result (1) := Sum1 / M_Length;
         Result (2) := Sum2 / M_Length;
      end loop;

      return Result;

   end Standard_Deviation;

   --  -------------------------------------------------------------------------

end NN_By_Hand_Support;
