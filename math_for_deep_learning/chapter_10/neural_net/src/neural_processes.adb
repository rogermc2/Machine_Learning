
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
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
     (Layer : in out Layer_Data; Out_Error : Layer_Vector) return Layer_Vector is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Processes.Backward ";
      Input_Length : constant Layer_Range := Layer.Input_Data'Length;
      In_Error     : Layer_Vector (Out_Error'Range);
      In_Data      : Layer_Matrix (1 .. 1, 1 .. Input_Length);
   begin
      for index in 1 .. Input_Length loop
         In_Data (1, index) := Layer.Input_Data (index);
      end loop;

      Put_Line (Routine_Name & "Layer Kind: " &
                  Layer_Type'Image (Layer.Layer_Kind));
      Put_Line (Routine_Name & "Out_Error Size" &
                  Integer'Image (Out_Error'Length));
      Put_Line (Routine_Name & "Layer.Input_Data" &
                  Integer'Image (Layer.Input_Data'Length));
      if Layer.Layer_Kind = Hidden_Layer then
         declare
            Weights_Error : constant Layer_Vector :=
                              Transpose (In_Data) * Out_Error;
         begin
            Print_Float_Vector (Routine_Name & "Weights_Error", Weights_Error);
            In_Error := Out_Error *
              Transpose (Layer.Weights);
            for row in Layer.Delta_W'Range loop
               for col in Layer.Delta_W'Range (2) loop
                  Layer.Delta_W (row,col) := Layer.Delta_W (row,col) +
                    Weights_Error (col);
               end loop;
               Layer.Bias (row) := Layer.Bias (row) + Out_Error (row);
            end loop;

            Layer.Passes := Layer.Passes + 1;
         end; --  declare block
      else
         In_Error := Out_Error;
      end if;

      return In_Error;

   end Backward;

   --  --------------------------------------------------------------

   function Forward
     (Layer : in out Layer_Data; Input_Data : Real_Float_List)
      return Real_Float_List is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Processes.Forward ";
      In_Data      : constant Real_Float_Vector :=
                       To_Real_Float_Vector (Input_Data);
      Out_Data     : Real_Float_List;
   begin
      Put_Line (Routine_Name & "Layer Kind: " &
                  Layer_Type'Image (Layer.Layer_Kind));
      --        Assert (Layer.Input_Data'Length = Integer (Input_Data.Length),
      --                Routine_Name & "Input_Data length" &
      --                  Integer'Image (Integer (Input_Data.Length)) &
      --                  " differs from layer Input_Data length" &
      --                  Integer'Image (Layer.Input_Data'Length));
      Put_Line (Routine_Name & "Layer Input_Data Size" &
                  Integer'Image (Layer.Input_Data'Length));
      if Layer.Layer_Kind = Hidden_Layer then
         Assert (Layer.Input_Data'Length = Integer (Input_Data.Length),
                 Routine_Name & "Input_Data length" &
                   Integer'Image (Integer (Input_Data.Length)) &
                   " differs from hidden layer Input_Data length" &
                   Integer'Image (Layer.Input_Data'Length));
         Print_Matrix_Dimensions
           (Routine_Name & "Layer.Weights", Layer.Weights);
         Layer.Input_Data := In_Data;
         Out_Data := To_Real_Float_List (Layer.Input_Data * Layer.Weights + Layer.Bias);

      else  --  Activation_Layer
         declare
            thisLayer : Neural_Processes.Layer_Data (Layer_Kind => Activation_Layer,
                                    Input_Size => 0,
                                    Output_Size => 0);
            Act_Layer : Neural_Processes.Layer_Data (Layer_Kind => Activation_Layer,
                                    Input_Size => Layer_Vector (In_Data'Length),
                                    Output_Size => 0);
         begin
            thisLayer := Act_Layer;
            Act_Layer.Input_Data := In_Data;
            thisLayer := Act_Layer;
            Layer := thisLayer;
         end;
         Out_Data := To_Real_Float_List (Neural_Maths.Sigmoid (In_Data));
      end if;

      return Out_Data;

   end Forward;

   --  --------------------------------------------------------------

   procedure Initialize
     (Layer : out Layer_Data; Input_Size, Output_Size : Positive) is
   begin
      for row in 1 .. Input_Size loop
         for col in 1 .. Output_Size loop
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

   function Loss (Y_True, Y_Pred : Real_Float_Vector) return Float is
      use Real_Float_Arrays;
   begin
      return 0.5 * Neural_Maths.Mean ((Y_True - Y_Pred) ** 2);

   end Loss;

   --  ------------------------------------------------------------------------

   function Loss_Deriv (Y_True, Y_Pred : Real_Float_Vector)
                        return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Loss_Deriv ";
   begin
      return Y_True - Y_Pred;

   end Loss_Deriv;

   --  --------------------------------------------------------------

   procedure Step (Layer : Layer_Data) is
   begin
      null;

   end Step;

   --  --------------------------------------------------------------

   procedure Step (Layer : in out Layer_Data; Eta : Float) is
      --        use Real_Float_Arrays;
      Eta_Av      : constant Float := Eta / Float (Layer.Passes);
   begin
      for row in Layer.Weights'Range loop
         for col in Layer.Weights'Range (2) loop
            Layer.Weights (row, col) :=
              Layer.Weights (row, col) - Eta_Av * Layer.Delta_W (row, col);
         end loop;

         Layer.Bias (row) :=
           Layer.Bias (row) - Eta_Av * Layer.Delta_B (row);
         Layer.Delta_W :=
           Zero_Matrix (Layer.Delta_W'Length, Layer.Delta_W'Length (2));
         Layer.Delta_B :=
           Zero_Array (Layer.Delta_B'Length);
      end loop;

      Layer.Passes := 0;

   end Step;

   --  --------------------------------------------------------------

end Neural_Processes;
