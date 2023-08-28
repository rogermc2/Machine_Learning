
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Shuffler;

with Structure; use Structure;

procedure Sequential is
   Program_Name : constant String := "Sequential ";
   --     X1 : Real_Float_Vector (1 .. 50);
   --     X2 : Real_Float_Vector (1 .. 50);
   --     Y1 : Real_Float_Vector (1 .. 50);
   --     Y2 : Real_Float_Vector (1 .. 50);

   X            : Real_Float_Matrix (1 .. 100, 1 ..2);
   Y            : Integer_Array (1 .. 100);
   X_Train      : Real_Float_Matrix (1 .. 100, 1 ..2);
   Y_Train      : Integer_Array (1 .. 100);
   X_Test       : Real_Float_Matrix (1 .. 100, 1 ..2);
   Y_Test       : Integer_Array (1 .. 100);

   Classifier           : Python.Module;

begin
   --  generate two data clusters.
   --     for index in X1'Range loop
   --        X1 (index) := Maths.Random_Float - 0.3;
   --        X2 (index) := Maths.Random_Float + 0.3;
   --        Y1 (index) := Maths.Random_Float + 0.3;
   --        Y2 (index) := Maths.Random_Float - 0.3;
   --     end loop;

   for row in X'Range loop
      if row <= 50 then
         X (row, 1) := Maths.Random_Float - 0.3;
         X (row, 2) := Maths.Random_Float + 0.3;
         Y (row) := 0;
      else
         X (row, 1) := Maths.Random_Float + 0.3;
         X (row, 2) := Maths.Random_Float - 0.3;
         Y (row) := 1;
      end if;

   end loop;

   Shuffler.Shuffle (X, Y);

   X_Train := Slice (X, 1, 75);
   X_Test := Slice (X, 76, 100);
   for row in Y'Range loop
      if row <= 75 then
         Y_Train (row) := Y (row);
      else
         Y_Test (row) := Y (row);
      end if;

   end loop;
   Python.Initialize;
   Classifier := Python.Import_File ("sequential");

   Python.Call (Classifier, "plot", X);

   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Sequential;
