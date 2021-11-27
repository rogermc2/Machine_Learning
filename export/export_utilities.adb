
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Export_Utilities is

    --  -------------------------------------------------------------------------

    function Arg_Max (Values : Colours_List) return Positive is
        Max_Value  : Float := Float'Safe_First;
        Max_Index  : Positive := 1;
        Colour     : Graph_Colours;
    begin
        for index in Values.First_Index .. Values.Last_Index loop
            Colour := Values.Element (index);
            if Colour.R > Max_Value then
                Max_Index := index;
                Max_Value := Colour.R;
            end if;
            if Colour.G > Max_Value then
                Max_Index := index;
                Max_Value := Colour.G;
            end if;
            if Colour.B > Max_Value then
                Max_Index := index;
                Max_Value := Colour.B;
            end if;
        end loop;

        return Max_Index;

    end Arg_Max;

    --  -------------------------------------------------------------------------

   procedure Get_Name (aLine  : in out Lines.Bounded_String;
                       Finish : in out Natural;  -- End of previous name,
                       --   return end of this name
                       N      : out Config.Name) is -- Return this name
      --  Maps for parsing
      --  If use Ada.Strings.Maps is not used, remember "use type" for "or"
      use Lines, Ada.Strings.Maps;
      --  Spaces include tabs and optional comments
      Space_Set : constant Character_Set :=
                    To_Set (" ," & Ada.Characters.Latin_1.HT);
      --  Words can include signed integer and decimal numbers
      Word_Set  :  constant Character_Set :=
                    Constants.Alphanumeric_Set or To_Set ("._");
      Punc_Set  :  constant Character_Set :=
                    To_Set ("{}[];=");
      Line_Size : Natural := Length (aLine);
      First     : Positive;   -- For call of Find_Token
   begin
      --  Get remaining input and trim spaces, tabs, commas
      Bounded_Slice (aLine, aLine, Finish + 1, Line_Size);
      Trim (aLine, Space_Set, Space_Set);
      Line_Size := Length (aLine);
      Finish := 0;
      --  End of line, do nothing
      if Line_Size = 0 then
         return;
         --  One char of punctuation
      elsif Is_In (Element (aLine, 1), Punc_Set) then
         N := Pad (Slice(aLine, 1, 1));
         Finish := 1;
         --  Edge pointer symbol
      elsif Line_Size > 1 and then Slice (aLine, 1, 2) = "->" then
         N := Pad (Slice (aLine, 1, 2));
         Finish := 2;
         --  Quoted name or attribute value
      elsif Element(aLine, 1) = '"' then
         Finish := Index (aLine, """", 2);
         --  Reserved word, element name, or attribute name or value
      elsif Is_In (Element(aLine, 1), Word_Set) then
         Find_Token (aLine, Word_Set, Ada.Strings.Inside, First, Finish);
         if Finish = 0 then return; end if;
      else
         return;
      end if;

      Ada.Strings.Fixed.Move (Slice (aLine, 1, Finish), N);

   end Get_Name;

   --  ------------------------------------------------------------------------

   --  Pad the string S so that it fits into Config.Name
   function Pad (S : in String) return Config.Name is
   begin
      return Ada.Strings.Fixed.Head (S, Config.Name'Length);
   end Pad;

end Export_Utilities;
