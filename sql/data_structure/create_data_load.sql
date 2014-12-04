SET NOCOUNT ON;

-- Create data_load schema
IF NOT EXISTS (SELECT schema_name FROM information_schema.schemata WHERE schema_name='data_load')
EXEC ('CREATE SCHEMA [data_load]')
GO


-- Add metadata for [data_load]
IF EXISTS(SELECT * FROM [db_meta].[data_dictionary] WHERE [ObjectType] = 'SCHEMA' AND [FullObjectName] = '[data_load]' AND [PropertyName] = 'MS_Description')
EXECUTE [db_meta].[drop_xp] 'data_load', 'MS_Description'

EXECUTE [db_meta].[add_xp] 'data_load', 'MS_Description', 'schema to hold all data load request objects'
GO


-- Create data load request table
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[data_load].[scenario_load_request]') AND type in (N'U'))
DROP TABLE [data_load].[scenario_load_request]
GO

CREATE TABLE [data_load].[scenario_load_request] (
	[scenario_load_request_id] [int] IDENTITY(1,1) NOT NULL,
	[scenario_year] [smallint] NOT NULL,
	[scenario_desc] [varchar](50) NOT NULL,
	[path] [varchar](200) NOT NULL,
	[iteration] [tinyint] NOT NULL,
	[sample_rate] [decimal](6,4) NOT NULL,
	[user_name] [varchar](50) NOT NULL,
	[date_requested] [smalldatetime] NOT NULL,
	[loading] [bit] NULL,
	[scenario_id] [smallint] NULL,
	CONSTRAINT pk_scenarioloadrequest PRIMARY KEY ([scenario_load_request_id])
	) 
ON 
	[ref_fg]
WITH 
	(DATA_COMPRESSION = PAGE);
GO


-- Add metadata for [data_load].[scenario_load_request]
EXECUTE [db_meta].[add_xp] 'data_load.scenario_load_request', 'SUBSYSTEM', 'data_load'
EXECUTE [db_meta].[add_xp] 'data_load.scenario_load_request', 'MS_Description', 'table holding scenarios to be loaded'
GO


-- Create stored procedure that populates data load request table
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[data_load].[sp_request]') AND type in (N'P', N'PC'))
DROP PROCEDURE [data_load].[sp_request]
GO

CREATE PROCEDURE [data_load].[sp_request] 
	@year smallint, @path nvarchar(200), @iteration tinyint, @sample_rate decimal(6,4) = 1
AS

DECLARE @desc nvarchar(50)
DECLARE @network_path nvarchar(200)
SET @desc = (SELECT REVERSE(SUBSTRING(REVERSE(@path), 0, CHARINDEX('\',REVERSE(@path)))))
SET @network_path = REPLACE(LOWER(@path), 't:', '\\hana\transdata')

INSERT INTO [data_load].[scenario_load_request]
VALUES (@year, @desc, @network_path, @iteration, @sample_rate, SYSTEM_USER, GETDATE(), 0, NULL)
GO


-- Add metadata for [data_load].[sp_request]
EXECUTE [db_meta].[add_xp] 'data_load.sp_request', 'SUBSYSTEM', 'data_load'
EXECUTE [db_meta].[add_xp] 'data_load.sp_request', 'MS_Description', 'stored procedure to populate data load request table'
GO