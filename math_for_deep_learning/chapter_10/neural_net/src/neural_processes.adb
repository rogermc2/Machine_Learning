
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
--  with Classifier_Utilities;
with ML_Types;
with Neural_Maths;
with Neural_Utilities;

package body Neural_Processes is

--     function Backward
--       (Layer : Layer_Data; Out_Error : Real_Float_Matrix)
--        return Real_Float_Matrix is
--        use Neural_Maths;
--        Result : Real_Float_Matrix (Layer.Input_Data'Range,
--                                    Layer.Input_Data'Range (2));
--     begin
--        for row in Layer.Input_Data'Range loop
--           for col in Layer.Input_Data'Range (2) loop
--              Result (row, col) :=
--                Sigmoid_Deriv (Layer.Input_Data (row, col)) * Out_Error (row, col);
--           end loop;
--        end loop;
--
--        return Result;

--     end Backward;

   --  --------------------------------------------------------------

   function Backward
     (Layer : in out Layer_Data; Out_Error : Real_Float_Vector)
      return Real_Float_Vector is
      use Real_Float_Arrays;
      In_Error      : constant Real_Float_Vector :=
                        Out_Error * Transpose (Layer.Weights);
      In_Data       : Real_Float_Matrix (1 .. 1, Layer.Input_Data'Range);
      Weights_Error : Real_Float_Vector (Layer.Input_Data'Range);
   begin
      for index in Layer.Input_Data'Range loop
         In_Data (1, index) := Layer.Input_Data (index);
      end loop;

      Weights_Error := Transpose (In_Data) * Out_Error;
      Layer.Delta_W := Layer.Delta_W + Weights_Error;
      Layer.Bias := Layer.Bias + Out_Error;
      Layer.Passes := Layer.Passes + 1;

      return In_Error;

   end Backward;

   --  --------------------------------------------------------------

   function Forward
     (Layer : in out Layer_Data; Input_Data : Real_Float_List)
      return Real_Float_List is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Processes.Forward ";
      Out_Data : Real_Float_List;
   begin
      Assert (Integer (Input_Data.Length) = Layer.Input_Data'Length,
              Routine_Name & "Input_Data Length" &
                  Integer'Image (Integer (Input_Data.Length)) &
                " is different to Layer Input_Data Length" &
                  Integer'Image (Layer.Input_Data'Length));
      Layer.Input_Data := To_Real_Float_Vector (Input_Data);
      declare
         New_Data : constant Real_Float_Vector :=
                      Layer.Input_Data * Layer.Weights + Layer.Bias;
      begin
         for index in New_Data'Range loop
           Out_Data.Append (New_Data (index));
         end loop;
      end;

      return Out_Data;

   end Forward;

   --  --------------------------------------------------------------

   procedure Initialize (Layer : out Layer_Data) is
   begin
      for row in Layer.Weights'Range loop
         for col in Layer.Weights'Range (2) loop
            Layer.Weights (row, col) := 0.5 * Maths.Random_Float;
         end loop;
         Layer.Bias (row) := 0.5 * Maths.Random_Float;
      end loop;

   end Initialize;

   --  --------------------------------------------------------------

   function Load_Data (File_Name : String; Num_Columns : Positive)
                       return Real_Float_Matrix is
      use Ada.Strings.Unbounded;
      CSV_Data  : constant ML_Types.Raw_Data_Vector :=
                    Neural_Utilities.Load_Raw_CSV_Data (File_Name);
      List_Row  : ML_Types.Unbounded_List;
      Data      : ML_Arrays_And_Matrices.Real_Float_Matrix
        (1 .. Positive (CSV_Data.Length), 1 .. Num_Columns);
   begin
      Put_Line ("Loading " & File_Name);
      for row in Data'Range loop
         List_Row := CSV_Data (row);
         for col in Data'Range( 2) loop
            Data (row, col) := Float'Value (To_String (List_Row (col)));
         end loop;
      end loop;

      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String) return Real_Float_Vector is
      use  Ada.Strings.Unbounded;
      CSV_Data : constant ML_Types.Unbounded_List :=
                   Neural_Utilities.Load_CSV_Data (File_Name);
      Data     : ML_Arrays_And_Matrices.Real_Float_Vector
        (1 .. Positive (CSV_Data.Length));
   begin
      Put_Line ("Loading " & File_Name);
      for index in CSV_Data.First_Index .. CSV_Data.Last_Index loop
         Data (index) := Float'Value (To_String (CSV_Data (index)));
      end loop;

      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

   function Loss (Y_True : Real_Float_Vector; Y_Pred : Real_Float_List)
                  return Float is
      use Real_Float_Arrays;
   begin
      return 0.5 * Neural_Maths.Mean
        ((Y_True - To_Real_Float_Vector (Y_Pred)) ** 2);

   end Loss;

   --  ------------------------------------------------------------------------

   function Loss_Deriv (Y_True : Real_Float_Vector; Y_Pred : Real_Float_List)
                        return Real_Float_Vector is
      use Real_Float_Arrays;
   begin
      return Y_True - To_Real_Float_Vector (Y_Pred);

   end Loss_Deriv;

   --  --------------------------------------------------------------

   procedure Step (Layer : Layer_Data) is
   begin
      null;

   end Step;

   --  --------------------------------------------------------------

   procedure Step (Layer : in out Layer_Data; Eta : Float) is
      use Real_Float_Arrays;
      Eta_Av : constant Float := Eta / Float (Layer.Passes);
   begin
      Layer.Weights :=
        Layer.Weights - Eta_Av * Layer.Delta_W;
      Layer.Bias :=
        Layer.Bias - Eta_Av * Layer.Delta_B;
      Layer.Delta_W :=
        Zero_Matrix (Layer.Delta_W'Length, Layer.Delta_W'Length (2));
      Layer.Delta_B :=
        Zero_Array (Layer.Delta_B'Length);
      Layer.Passes := 0;

   end Step;

   --  --------------------------------------------------------------

end Neural_Processes;
