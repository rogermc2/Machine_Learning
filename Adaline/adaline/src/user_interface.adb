
with Ada.Text_IO; use Ada.Text_IO;

package body User_Interface is

   procedure Introduce_Patterns is
   begin
      null;
   end Introduce_Patterns;

   --  ------------------------------------------------------------------------

   procedure Read_Patterns_File is
   begin
      null;
   end Read_Patterns_File;

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
      Put_Line ("** CHANGE TRAINING PATTERN * *");
      while not Ok and Count < 3 loop
         Count := Count + 1;
         Put_Line (" [1] enter patterns manually.");
         Put_Line (" [2] read patterns from file.");
         Put_Line (" [3] read weights already calculated and do not train");
         Put_Line (" [4] write training pattern to file");
         Put_Line (" [0] return to main menu.");
         Put ("Enter option: ");
         Get (Key);
         case Key is
         when '0' => Ok := True;
         when '1' => Ok := True; --  enter patterns;
         when '2' =>
            Ok := True;
            Read_Patterns_File;
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
