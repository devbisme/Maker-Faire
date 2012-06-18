--*********************************************************************
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--
-- ©2012 - X Engineering Software Systems Corp. (www.xess.com)
--*********************************************************************


--*********************************************************************
-- Module for driving StickIt! seven-segment LED string.
--*********************************************************************


library IEEE;
use IEEE.std_logic_1164.all;
use work.CommonPckg.all;

package LedCirclePckg is

  component LedCircleDisplay is
    generic (
      FREQ_G        : real := 100.0;    -- Operating frequency in MHz.
      UPDATE_FREQ_G : real := 1.0  -- Desired update frequency for the entire LED display in KHz.
      );
    port (
      clk_i        : in  std_logic;     -- Input clock.
      ledAll_i     : in  std_logic_vector(89 downto 0) := (others => ZERO);
      -- These are the 3-state drivers for the LED digits.
      ledDrivers_o : out std_logic_vector (9 downto 0)
      );
  end component;

end package;




--**************************************************************************************************
-- This module outputs a set of LED activation bit vectors to a charlieplexed string of LEDs.
--**************************************************************************************************

library IEEE, UNISIM;
use IEEE.MATH_REAL.all;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use UNISIM.vcomponents.all;
use work.CommonPckg.all;

entity LedCircleDisplay is
  generic (
    FREQ_G        : real := 100.0;      -- Operating frequency in MHz.
    UPDATE_FREQ_G : real := 1.0  -- Desired update frequency for the entire LED display in KHz.
    );
  port (
    clk_i        : in  std_logic;       -- Input clock.
    ledAll_i     : in  std_logic_vector(89 downto 0) := (others => ZERO);
    -- These are the 3-state drivers for the LED digits.
    ledDrivers_o : out std_logic_vector (9 downto 0)
    );
end entity;

architecture arch of LedCircleDisplay is
  signal arcShf_r   : unsigned(ledDrivers_o'range) := "0000000001";  -- Shift reg indicates which digit is active.
  signal segShf_r   : unsigned(ledDrivers_o'range) := "1111111111";  -- Shift reg indicates which LED segments are active.
  signal segments_s : std_logic_vector(ledAll_i'range);  -- 1 indicates segment is on, 0 means off.
  signal cathodes_s : std_logic_vector(8 downto 0);  -- Cathode levels for the LEDs of the active digit.
  signal tris_s     : std_logic_vector(ledDrivers_o'range);  -- Output driver tristate settings.
begin

  -- Shift the active LED segment every SEG_PERIOD_C clock cycles, and shift the active band after every ten shifts of the LED segment.
  process(clk_i)
    constant SEG_PERIOD_C : natural := integer(ceil(FREQ_G * 1000.0 / (UPDATE_FREQ_G * real(ledAll_i'length))));
    variable segTimer_v   : natural range 0 to SEG_PERIOD_C;
    variable segCntr_v    : natural range ledDrivers_o'range;
  begin
    if rising_edge(clk_i) then
      if segTimer_v /= 0 then  -- The timer period for this segment has not expired.
        segTimer_v := segTimer_v - 1;   -- Decrement LED segment timer.
      else                              -- The LED segment timer has expired.
        segShf_r   <= segShf_r rol 1;  -- Shift to the next segment of the digit.
        segTimer_v := SEG_PERIOD_C;     -- Restart the LED segment timer.
        if segCntr_v /= 0 then  -- If all the segments in this digit are not done...
          segCntr_v := segCntr_v - 1;  -- ... decrement digit counter until it reaches 0.
        else  -- Else, all the segments in this digit are done so.
          arcShf_r  <= arcShf_r rol 1;  -- Shift to next digit.
          segCntr_v := ledDrivers_o'high;  -- Restart the segment counter.
        end if;
      end if;
    end if;
  end process;
  
  segments_s <= ledAll_i;

  -- Select a slice of the total LED segment activation vector corresponding to the LEDs for the currently active digit.
  -- The cathode level will be low for each active segment in the digit.
  process(arcShf_r, segments_s)
  begin
    case arcShf_r is
      when "0000000001" => cathodes_s <= not segments_s(8 downto 0);
      when "0000000010" => cathodes_s <= not segments_s(17 downto 9);
      when "0000000100" => cathodes_s <= not segments_s(26 downto 18);
      when "0000001000" => cathodes_s <= not segments_s(35 downto 27);
      when "0000010000" => cathodes_s <= not segments_s(44 downto 36);
      when "0000100000" => cathodes_s <= not segments_s(53 downto 45);
      when "0001000000" => cathodes_s <= not segments_s(62 downto 54);
      when "0010000000" => cathodes_s <= not segments_s(71 downto 63);
      when "0100000000" => cathodes_s <= not segments_s(80 downto 72);
      when "1000000000" => cathodes_s <= not segments_s(89 downto 81);
      when others       => cathodes_s <= (others => HI);
    end case;
  end process;

  -- Connect the cathode levels to the cathode drivers of the active digit and activate the driver for every
  -- cathode at a low level. Tristate the driver for cathodes at a high level. Also, activate the driver
  -- for the anode pin of the active LED digit. The anode for LED digit i is at signal index i. The cathodes
  -- connect to all the other indices.
  process(arcShf_r, segShf_r, cathodes_s)
    variable j : natural range arcShf_r'range := 0;
  begin
    j      := 0;
    tris_s <= not std_logic_vector(arcShf_r);  -- Start off by tristating everything except the current digit's anode.
    for i in arcShf_r'low to arcShf_r'high loop
      if arcShf_r(i) = LO then  -- Process only the cathodes of the active digit which have low levels in the digit shift register. Skip the anode.
        if segShf_r(i) = HI and cathodes_s(j) = LO then  -- Activate tristate driver if the segment is active and the cathode level is low.
          tris_s(i) <= LO;              -- Turn tristate off and driver on.
        end if;
        j := j + 1;                     -- Move to the next cathode bit.
      end if;
    end loop;
  end process;

  -- Instantiate the tristate drivers. The active digit shift register is attached to the driver inputs so only the anode of the currently
  -- active LED digit is driven high. The other drivers will pull the cathode pins low if the corresponding LED segment is active.
  ObuftLoop : for i in ledDrivers_o'low to ledDrivers_o'high generate
    UObuft : OBUFT generic map(DRIVE => 24, IOSTANDARD => "LVTTL") port map(T => tris_s(i), I => arcShf_r(i), O => ledDrivers_o(i));
  end generate;
  
end architecture;
