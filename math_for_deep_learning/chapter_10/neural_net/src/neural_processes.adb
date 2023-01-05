
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Neural_Maths;
with Neural_Utilities;

package body Neural_Processes is

   procedure Backward
     (Layer : in out Layer_Data; Error : in out Real_Float_List) is
      use Real_Float_Arrays;
--        Routine_Name  : constant String := "Neural_Processes.Backward ";
      Data_Mat      : constant Real_Float_Matrix :=
                        Real_Float_Matrix (Layer.Input_Data);
      Error_Mat     : constant Real_Float_Matrix :=
                        To_Real_Float_Matrix (Error);
   begin
      --        Put_Line (Routine_Name & "Layer Kind: " &
      --                    Layer_Type'Image (Layer.Layer_Kind));
      --        Print_Matrix_Dimensions (Routine_Name & "Data_Mat", Data_Mat);
      --        Print_Matrix_Dimensions (Routine_Name & "Error_Mat", Error_Mat);
      if Layer.Layer_Kind = Hidden_Layer then
         declare
            In_Error      : constant Real_Float_Matrix :=
                              Error_Mat * Transpose
                                (Real_Float_Matrix (Layer.Weights));
            Weights_Error : constant Real_Float_Matrix :=
                              Transpose (Data_Mat) * Error_Mat;
         begin
            --              Print_Matrix_Dimensions (Routine_Name & "Layer.Weights",
            --                                       Real_Float_Matrix (Layer.Weights));

            --           Print_Float_Matrix (Routine_Name & "Weights_Error", Weights_Error);

            --              Print_Float_Matrix (Routine_Name & "In_Error", In_Error, 1, 1, 1, 8);
            --  accumulate the error over a minibatch
            Layer.Delta_W :=
              Layer_Matrix (Real_Float_Matrix (Layer.Delta_W) + Weights_Error);

            for index in Layer.Bias'Range (2) loop
               Layer.Bias (1, index) :=
                 Layer.Bias (1, index) + Error (Integer (index));
            end loop;
            Layer.Passes := Layer.Passes + 1;

            Error.Clear;
            for col in In_Error'Range (2) loop
               Error.Append (In_Error (1, col));
            end loop;
         end;

      else  --  Activation layer
         declare
            In_Error : constant Real_Float_Matrix
              := H_Product (Neural_Maths.Sigmoid_Deriv (Data_Mat), Error_Mat);
         begin
            Error.Clear;
            for col in In_Error'Range (2) loop
               Error.Append (In_Error (1, col));
            end loop;
            --              Print_Real_Float_List (Routine_Name & "Activation Error",
            --                                     Error, 1, 8);
         end;
      end if;

   end Backward;

   --  --------------------------------------------------------------

   procedure Forward
     (Layer : in out Layer_Data; Data : in out Real_Float_List) is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Forward ";
      In_Data      : constant Real_Float_Matrix :=
                       To_Real_Float_Matrix (Data);
      Out_Data     : Real_Float_List;
   begin
      --        Put_Line (Routine_Name & "Layer Kind: " &
      --                    Layer_Type'Image (Layer.Layer_Kind));
      Layer.Input_Data := Layer_Matrix (In_Data);
      if Layer.Layer_Kind = Hidden_Layer then
         declare
            Out_Mat : constant Real_Float_Matrix :=
                        In_Data * Real_Float_Matrix (Layer.Weights) +
                        Real_Float_Matrix (Layer.Bias);
         begin
            for col in Out_Mat'Range (2) loop
               Out_Data.Append (Out_Mat (1, col));
            end loop;
         end;

      else  --  Activation_Layer
         for col in In_Data'Range (2) loop
            Out_Data.Append (Neural_Maths.Sigmoid (In_Data) (1, col));
         end loop;
      end if;

      Data := Out_Data;
--        Print_Real_Float_List ("Data", Data, 1, 2);

   end Forward;

   --  --------------------------------------------------------------

   procedure Initialize
     (Layer : out Layer_Data; Input_Size, Output_Size : Layer_Range) is
   begin
      for row in 1 .. Input_Size loop
         for col in 1 .. Output_Size loop
            Layer.Weights (row, col) := 0.5 * Maths.Random_Float;
         end loop;
         Layer.Bias (1, row) := 0.5 * Maths.Random_Float;
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

   function Mean_Square_Error (Y_True, Y_Pred : Real_Float_Vector) return Float is
      use Real_Float_Arrays;
   begin
      return 0.5 * Neural_Maths.Mean ((Y_True - Y_Pred) ** 2);

   end Mean_Square_Error;

   --  ------------------------------------------------------------------------

   function Minus_MSE_Derivative (Y_True, Y_Pred : Real_Float_Vector)
                                  return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Loss_Deriv ";
   begin
      return Y_Pred - Y_True;

   end Minus_MSE_Derivative;

   --  --------------------------------------------------------------

   procedure Step (Layer : in out Layer_Data; Eta : Float) is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Step Hidden_Layer ";
   begin
      if Layer.Layer_Kind = Hidden_Layer then
         declare
            Eta_Av : constant Float := Eta / Float (Layer.Passes);
            ZMW    : constant Real_Float_Matrix :=
                       Zero_Matrix (Layer.Delta_W'Length,
                                    Layer.Delta_W'Length (2));
            ZMD    : constant Real_Float_Matrix :=
                       Zero_Matrix (Layer.Delta_B'Length,
                                    Layer.Delta_B'Length (2));
         begin
            Layer.Weights :=
              Layer_Matrix (Real_Float_Matrix (Layer.Weights) -
                                Eta_Av * Real_Float_Matrix (Layer.Delta_W));

            Layer.Bias :=
              Layer_Matrix (Real_Float_Matrix (Layer.Bias) -
                                Eta_Av * Real_Float_Matrix (Layer.Delta_B));
            Layer.Delta_W := Layer_Matrix (ZMW);
            Layer.Delta_B := Layer_Matrix (ZMD);
            Layer.Passes := 0;
         end;  -- declare block
      end if;  --  Hidden layer

   end Step;

   --  --------------------------------------------------------------

end Neural_Processes;
