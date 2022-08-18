
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with NL_Types;

package body Training is

   Float_Gen : Ada.Numerics.Float_Random.Generator;

   --  ------------------------------------------------------------------------

   procedure Calc_Output (Neuron : in out Processing_Element) is
   begin
      null;

   end Calc_Output;

   --  ------------------------------------------------------------------------

   procedure Calculate (Num_Layers : Positive; EMC : in out Float;
                        Errors     : Output_Type) is
   begin
      for index in 1 .. Num_Layers loop
         EMC := EMC + errors (index);
      end loop;

      EMC := EMC / Float (Num_Layers);

   end Calculate;

   --  ------------------------------------------------------------------------

   procedure Change_Weights (Neuron : in out Processing_Element) is
   begin
      null;
   end ;

   --  ------------------------------------------------------------------------

   function Enter_Layers (Num_Data, Num_Layers    : out Positive;
                           Layer_Entries           : out Layer_List;
                           Active_Layer            : out Boolean) return Output_Type is
   begin
      Put ("Enter number of layers: ");
      Get (Num_Layers);
      Put ("Enter number of entries: ");
      Get (Num_Data);
      declare
         Data        : Data_Type (1 .. Num_Data);
         Output_Data : Output_Type (1 .. Num_Layers);
      begin
         for layer_index in 1 .. Num_Layers loop
            for data_index in 1 .. Num_Data loop
               Put ("Enter data for layer" & Integer'Image (layer_index) &
                      ", input"  & Integer'Image (data_index) & ": " );
               Get (Data (data_index));
            end loop;

            Layer_Entries.Append (Data);

            Put ("Enter desired output: ");
            Get (Output_Data (layer_index));
         end loop;

         Active_Layer := True;
         return Output_Data;
      end;  --  declare block

   end Enter_Layers;

   --  ------------------------------------------------------------------------

   function Initialize_Training
     (Num_Data, Num_Layers  : out Natural;
      Layer_Entries         : out Layer_List;
      Active_Layer          : out Boolean) return Output_Type is
      Key   : Character;
      Count : Natural := 0;
      Ok    : Boolean := False;
      Quit  : Boolean := False;
      Dummy : Output_Type (1 .. 0);
   begin
      Put_Line ("** CHANGE TRAINING Layer **");
      while not Ok and Count < 3 and not Quit loop
         Count := Count + 1;
         Put_Line (" 0: return to main menu.");
         Put_Line (" 1: enter layers manually.");
         Put_Line (" 2: read layers from file.");
         Put_Line (" 3: read weights already calculated and do not train");
         Put_Line (" 4: write training layer to file");
         Put ("Enter option: ");
         Get (Key);
         case Key is
            when '0' =>
               Quit := True;
               return Dummy;
            when '1' =>
               Ok := True;
               return Enter_Layers (Num_Data, Num_Layers, Layer_Entries,
                                    Active_Layer);
            when '2' => Ok := True;  --  readLayersfile;
               return Dummy;
            when '3' => Ok := True;  --  readweightsfile;
               return Dummy;
            when '4' => Ok := True;  --   save fileLayer;
               return Dummy;
            when others =>
               Put_Line ("Invalid option!");
         end case;
      end loop;

      return Dummy;

   end Initialize_Training;

   --  ------------------------------------------------------------------------

   procedure Init_Neuron (Neuron               : in out Processing_Element;
                          Num_Data, Num_Layers : Positive) is
      use Ada.Numerics.Float_Random;
   begin
      for row in 1 .. Num_Data loop
         Neuron.Weights (row) := 3.0 * Float (Random (Float_Gen));
         Neuron.Entries (row) := 0.0;
      end loop;

      Neuron.Trend := 3.0 * Float (Random (Float_Gen));
      Neuron.Activ := 0.0;
      Neuron.Output := 0.0;
      Neuron.Trained := False;

   end Init_Neuron;

   --  ------------------------------------------------------------------------

   procedure Set_Layer (Neuron   : in out Processing_Element;
                        Num_Data : Positive;
                        Layer    : Data_Type;
                        Errors   : in out Output_Type) is
      --  received Layer, desired output
      use Maths.Float_Math_Functions;
      Current_Error : Float;
   begin
      for row in Neuron.Entries'Range loop
         Neuron.Entries (row) := Layer (row);
      end loop;

      --  Calculate The Output
      Calc_Output (Neuron);
      Current_Error := Neuron.Output;
      Errors (Layer'Length) := Sqrt (Current_Error);

   end Set_Layer;

   --  ------------------------------------------------------------------------

   procedure Train_Neuron
     (Neuron                                    : in out Processing_Element;
      Num_Data, Num_Layers                      : in out Natural;
      Active_Layer, Record_Weights, Write_Error : Boolean) is
      End_Training       : Boolean;
      Num_Interactions   : Natural := 0;
      Emc                : Float;
      Option             : Character;
      Layers             : Layer_List;
      Layer_Count        : Integer;
      Error_File_Name    : Unbounded_String;
      Weights_File_Names : NL_Types.Data_Rows (1 .. Num_Data);
      Error_File_ID      : File_Type;
      Weights_Files      : array (1 .. Num_Data) of File_Type;
      Real_List          : NL_Types.Float_List; --  Weights stored in time
      Output             : Output_Type (1 .. Num_Data);
      Errors             : Output_Type (1 .. Num_Data) := (others => 0.0);
      Interactions       : Natural := 0;
   begin
      if not Active_Layer then
         Put_Line ("There Is No Training Layer Loaded.");
      else
         Put ("Enter Name Of Emc File: ");
         Get_Line (Error_File_Name);

         Open (Error_File_ID, Out_File, To_String (Error_File_Name));
         --           Rewrite (Errorfile);

         Init_Neuron (Neuron, Num_Data, Num_Layers);
         if Record_Weights then
            for index in 1 .. Num_Data loop
               Put ("Enter Weights File Name (" & Integer'Image (index) &
                      "): ");
               Get_Line (Weights_File_Names (index));
            end loop;

            Real_List.Clear;
            for index in 1 .. Num_Data loop
               Real_List.Append (Neuron.Weights (index));
            end loop;
         end if;

         Put (" Want Trend (Y/N) ? ");
         Get (Option);
         Neuron.Tendencon := Option = 'S';

         Num_Interactions := 0;
         Emc := 0.0;
         End_Training := False;
         Put_Line ("Training Neuron.");
         while not End_Training loop
            Layer_Count := 1;
            while Layer_Count <= Num_Layers loop
               Set_Layer (Neuron, Num_Data, Layers (Layer_Count),
                          Output);
               Change_Weights (Neuron);
               Layer_Count := Layer_Count + 1;
            end loop;
            Interactions := Interactions + 1;
            Calculate (Num_Layers, EMC, Errors);

            Put_Line ("In The Interaction " & Integer'Image (Interactions) &
                        " the Emc Has Been "); --  & Emc);
            if Write_Error Then
               Put (Error_File_ID, Emc);
            end if;

            if Record_Weights then
               for index in 1 .. Num_Data loop
                  Real_List.Append (Neuron.Weights (index));
               end loop;
            end if;

            if Emc = 0.0 then
               End_Training := True;
               --  Exit Desired: ',Salidad[I]:5:3);
            end if;
         end loop;
      end if;

   end Train_Neuron;

   --  ------------------------------------------------------------------------

end Training;
