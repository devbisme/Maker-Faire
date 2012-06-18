----------------------------------------------------------------------------------
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
-- �2011-2012 - X Engineering Software Systems Corp. (www.xess.com)
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Modules for generating a clock frequency from a master clock and for transferring
-- a clock signal from the clock network to a logic input or an output pin.
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.all;

package ClkGenPckg is

component ClkGen is
  generic (
    BASE_FREQ_G : real                  := 12.0;  -- Input frequency in MHz.
    CLK_MUL_G   : natural range 1 to 32 := 25;    -- Frequency multiplier.
    CLK_DIV_G   : natural range 1 to 32 := 3      -- Frequency divider.
    );
  port (
    i   : in  std_logic;                -- Clock input (12 MHz by default).
    o   : out std_logic;  -- Generated clock output (100 MHz by default).
    o_b : out std_logic  -- Negative-phase generated clock output (inverse of 'o' output).
    );
end component;

component ClkToLogic is
  port (
    clk_i  : in  std_logic;             -- Positive-phase of clock input.
    clk_ib : in  std_logic;             -- Negative-phase of clock input.
    clk_o  : out std_logic   -- Clock output that's suitable as a logic input.
    );
end component;

end package;


library IEEE, UNISIM;
use IEEE.STD_LOGIC_1164.all;
use work.CommonPckg.all;
use UNISIM.VComponents.all;

entity ClkGen is
  generic (
    BASE_FREQ_G : real                  := 12.0;  -- Input frequency in MHz.
    CLK_MUL_G   : natural range 1 to 32 := 25;    -- Frequency multiplier.
    CLK_DIV_G   : natural range 1 to 32 := 3      -- Frequency divider.
    );
  port (
    i   : in  std_logic;                -- Clock input (12 MHz by default).
    o   : out std_logic;  -- Generated clock output (100 MHz by default).
    o_b : out std_logic  -- Negative-phase generated clock output (inverse of 'o' output).
    );
end entity;

architecture arch of ClkGen is
  signal genClkP_s : std_logic;
  signal genClkN_s : std_logic;
begin

  u0 : DCM_SP
    generic map (
      CLKDV_DIVIDE          => 2.0,
      CLKFX_DIVIDE          => CLK_DIV_G,  --  Can be any interger from 1 to 32
      CLKFX_MULTIPLY        => CLK_MUL_G,  --  Can be any integer from 1 to 32
      CLKIN_DIVIDE_BY_2     => false,  --  TRUE/FALSE to enable CLKIN divide by two feature
      CLKIN_PERIOD          => 1000.0 / BASE_FREQ_G,  --  Specify period of input clock in ns
      CLKOUT_PHASE_SHIFT    => "NONE",  --  Specify phase shift of NONE, FIXED or VARIABLE
      CLK_FEEDBACK          => "NONE",  --  Specify clock feedback of NONE, 1X or 2X
      DESKEW_ADJUST         => "SYSTEM_SYNCHRONOUS",
      DLL_FREQUENCY_MODE    => "LOW",   --  HIGH or LOW frequency mode for DLL
      DUTY_CYCLE_CORRECTION => true,   --  Duty cycle correction, TRUE or FALSE
      PHASE_SHIFT           => 0,  --  Amount of fixed phase shift from -255 to 255
      STARTUP_WAIT          => false)  --  Delay configuration DONE until DCM LOCK, TRUE/FALSE
    port map (
      RST      => '0',                  -- DCM asynchronous reset input
      CLKIN    => i,               -- Clock input (from IBUFG, BUFG or DCM)
      CLKFX    => o,               -- Positive-phase of generated clock output
      CLKFX180 => o_b              -- Negative-phase of generated clock output.
      );
end architecture;


library IEEE, UNISIM;
use IEEE.STD_LOGIC_1164.all;
use work.CommonPckg.all;
use UNISIM.VComponents.all;

entity ClkToLogic is
  port (
    clk_i  : in  std_logic;             -- Positive-phase of clock input.
    clk_ib : in  std_logic;             -- Negative-phase of clock input.
    clk_o  : out std_logic   -- Clock output that's suitable as a logic input.
    );
end entity;

architecture arch of ClkToLogic is
begin
  -- Use ODDR2 to transfer clock signal from FPGA's clock network to the logic fabric.
  -- (This stops the synthesis tools from complaining about using a clock as an input
  -- to a logic gate or when driving a pin for an external clock signal.)
  u1 : ODDR2
    port map (
      Q  => clk_o,
      C0 => clk_i,
      C1 => clk_ib,
      CE => YES,
      D0 => ONE,
      D1 => ZERO,
      R  => ZERO,
      S  => ZERO
      );
end architecture;
