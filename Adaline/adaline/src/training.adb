
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Training is

   procedure Initialize_Training is
      Key   : Character;
      Count : Natural := 0;
      Ok    : Boolean := False;
   begin
      Put_Line ("** CHANGE TRAINING PATTERN **");
      while not Ok and Count < 3 loop
         Count := Count + 1;
         Put_Line (" [1] enter patterns manually.");
         Put_Line (" [2] read patterns from file.");
         Put_Line (" [3] read weights already calculated and do not train");
         Put_Line (" [4] write training pattern to file");
         Put_Line (" [0] return to main menu.");
         Put ("Enter option:");
         Get (Key);
         case Key is
         when '1' => Ok := True;  --  enter patterns;
         when '2' => Ok := True;  --  readpatternsfile;
         when '3' => Ok := True;  --  readweightsfile;
         when '4' => Ok := True;  --   save filepattern;
         when others =>
            Put_Line ("Invalid option!");
         end case;
      end loop;

   end Initialize_Training;

   --  ------------------------------------------------------------------------

   procedure Enter_Patterns (Num_Data, Num_Patterns  : in out Positive;
                             Pattern_Entries         : out Pattern_Type;
                             Output_Data             : out Output_Type;
                             Active_Pattern          : in out Boolean) is
   begin
      Put ("Enter number of patterns: ");
      Get (Num_Patterns);
      Put ("Enter number of entries: ");
      Get (Num_Data);

      declare
         Patterns : Pattern_Type (1 .. Num_Patterns, 1 .. Num_Data);
      begin
         for patt_index in 1 .. Num_Patterns loop
            for data_index in 1 .. Num_Data loop
               Put ("Enter pattern data (" & Integer'Image (patt_index) & ","
                    & Integer'Image (data_index) & "):" );
               Get (Patterns (patt_index, data_index));
            end loop;
            Pattern_Entries := Patterns;

            Put ("Enter desired output: ");
            Get (Output_Data (patt_index));
         end loop;
      end;  --  declare block

      Active_Pattern := True;

   end Enter_Patterns;

   --  ------------------------------------------------------------------------

   procedure Train_Neuron is
   begin
      null;
   end Train_Neuron;

   --  ------------------------------------------------------------------------

end Training;
