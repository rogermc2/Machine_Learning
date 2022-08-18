
with Ada.Text_IO; use Ada.Text_IO;

package body User_Interface is

   procedure Introduce_Layers is
   begin
      null;
   end Introduce_Layers;

   --  ------------------------------------------------------------------------

   procedure Read_Layers_File is
   begin
      null;
   end Read_Layers_File;

   --  ------------------------------------------------------------------------

   procedure Read_Weights_File is
   begin
      null;
   end Read_Weights_File;

   --  ------------------------------------------------------------------------

   procedure Save_File is
   begin
      null;
   end Save_File;

   --  ------------------------------------------------------------------------

   procedure Training_Intro is
      Key   : Character;
      Count : Natural := 0;
      Ok    : Boolean := False;
   begin
      Put_Line ("** CHANGE TRAINING Layer * *");
      while not Ok and Count < 3 loop
         Count := Count + 1;
         Put_Line (" [1] enter layers manually.");
         Put_Line (" [2] read layers from file.");
         Put_Line (" [3] read weights already calculated and do not train");
         Put_Line (" [4] write training layer to file");
         Put_Line (" [0] return to main menu.");
         Put ("Enter option: ");
         Get (Key);
         case Key is
         when '0' => Ok := True;
         when '1' => Ok := True; --  enter Layers;
         when '2' =>
            Ok := True;
            Read_Layers_File;
         when '3' =>
            Ok := True;
            Read_Weights_File;
         when '4' =>
            Ok := True;
            Save_File;
         when others =>
            Put_Line ("Invalid option!");
         end case;
      end loop;

   end Training_Intro;

   --  ----------------------------------------------

end User_Interface;
