
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

package body Validation is

   Type Attribute_Cursor is new Attribute_Package.Cursor;

   --  Check_Is_Fitted checks if the estimator is fitted by verifying the
   --  presence of fitted attributes (ending with a trailing underscore).
   --  Otherwise raises a Not_Fitted_Error with the given message.
   procedure Check_Is_Fitted
     (Estimator  : Attribute_List;
      Attributes : Attribute_Package.List := Attribute_Package.Empty_List;
      Msg        : Unbounded_String := Empty_String;
      All_Or_Any : State := All_States) is

      use Attribute_Package;
      use Ada.Strings;
      Cursor     : Attribute_Cursor := Attribute_Cursor (Estimator.First);
      Attributes_Length : constant Integer := Integer (Attributes.Length);
      Attr       : Attribute_List;
   begin
      if Is_Empty (Estimator) then
         raise Not_Fitted_Error with
           "Validation.Check_Is_Fitted; Estimator is empty ";
      elsif Msg = Empty_String then
         Put_Line ("Validation.Check_Is_Fitted, this " &
                   To_String (Estimator.First_Element) & " instance is not fitted yet.");
         Put_Line ("Call 'fit' with appropriate arguments before using this estimator.");
      elsif Estimator.First_Element /= "Fit" then
         raise Not_Fitted_Error with "Validation.Check_Is_Fitted invalid estimator";
      elsif Is_Empty (Attributes) then
         raise Not_Fitted_Error with "Validation.Check_Is_Fitted ";
      else
         --  Check if the estimator is fitted by verifying the presence of
         --  fitted attributes (ending with a trailing underscore) and otherwise
         --  raise a NotFittedError with the given message.
         while Has_Element (Cursor) loop
            declare
               Est : String := To_String (Element (Cursor));
            begin
              if Fixed.Index (Est, "_") = Est'Last and not
                    (Est (1 .. 2) = "__") then
                  Attr.Append (To_Unbounded_String (Est));
              end if;
            end;
            Next (Cursor);
         end loop;
      end if;

      if Is_Empty (Attr) then
         raise Not_Fitted_Error with "Validation.Check_Is_Fitted; " &
           To_String (Msg);
      end if;
   end Check_Is_Fitted;

end Validation;
