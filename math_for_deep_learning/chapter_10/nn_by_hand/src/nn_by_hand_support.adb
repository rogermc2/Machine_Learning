
with Maths;

with Load_Dataset;
with ML_Types;
with NL_Types;

package body NN_By_Hand_Support is

   procedure Mean (Row : in out Real_Float_Vector);

   --  -------------------------------------------------------------------------

   procedure Build_Dataset (X_Train, Y_Train,
                            X_Test, Y_Test : out Real_Float_Vector) is
      use maths.Float_Math_Functions;
      Iris_Data        : constant Load_Dataset.Iris_Data_Record :=
                           Load_Dataset.Load_Iris ("src/iris.csv");
      Features         : constant NL_Types.Float_List_2D := Iris_Data.Features;
      Target           : constant ML_Types.Integer_List := Iris_Data.Target;
      Num_Features     : constant Positive := Iris_Data.Num_Features;
      Feature_Row      : NL_Types.Float_List;
      X                : Real_Float_Matrix
        (Features.First_Index .. Features.Last_Index, 1 ..2);
      X_Row            : Real_Float_Vector (1 .. 2);
   begin
      for row in X'Range loop
         Feature_Row := Features (row);
         X (row, 1) := Feature_Row (1);
         X (row, 2) := Feature_Row (2);
      end loop;

      for row in X'Range loop
         X_Row (1) := X (row, 1);
         X_Row (2) := X (row, 2);
         Mean (X_Row);

         X (row, 1) := X_Row (1);
         X (row, 2) := X_Row (2);
      end loop;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   procedure Mean (Row : in out Real_Float_Vector) is
      Sum : Float := 0.0;
   begin
      for index in Row'Range loop
         Sum := Sum + Row (index);
      end loop;

      for index in Row'Range loop
         Row (index) := Row (index) / Sum;
      end loop;

   end Mean;

end NN_By_Hand_Support;
