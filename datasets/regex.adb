
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Regex is

   procedure Find_Match
      (Compiled_Expression : Gnat.Regpat.Pattern_Matcher; Text : String;
       First, Last : out Positive; Found : out Boolean) is
      use Gnat.Regpat;
      Result : Match_Array (0 .. 1);
    begin
        --  Match selects the first substring of Text that matches
        --  Compiled_Expression
        Match (Compiled_Expression, Text, Result);
        Found := Result (1) /= No_Match;

        if Found then
            First := Result (1).First;
            Last := Result (1).Last;
        end if;

   end Find_Match;

   --  -------------------------------------------------------------------------

   function Get_Groups (Regex : String) return String is
      use Gnat.Regpat;
      Pattern  : constant String := "c";
      Matcher  : constant Pattern_Matcher := Compile (Pattern);
      UB_Regex : constant Unbounded_String := To_Unbounded_String (Regex);
      First    : Positive;
      Last     : Positive;
      Found    : Boolean := True;
      Groups   : Unbounded_String := To_Unbounded_String ("");
   begin
      while Found loop
         Find_Match (Matcher, Regex, First, Last, Found);
         if Found then
            Put_Line ("Group found");
            Groups := Groups & Slice (UB_Regex, First, Last);
         end if;
         Put_Line (Groups);
      end loop;

      return To_String (Groups);

   end Get_Groups;

   --  -------------------------------------------------------------------------

end Regex;
