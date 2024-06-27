<?xml version="1.0" encoding="utf-8"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v3" manifestVersion="1.0" description="Fix for KB4577066" displayName="default" company="Microsoft Corporation" copyright="Microsoft Corporation" supportInformation="http://support.microsoft.com/?kbid=4577066" creationTimeStamp="2020-09-03T10:36:01Z" lastUpdateTimeStamp="2020-09-03T10:36:01Z">
	<assemblyIdentity name="Package_980_for_KB4577066" version="6.3.1.11" language="neutral" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35"/>
	<package identifier="KB4577066" releaseType="Security Update" restart="possible" psfName="Windows8.1-KB4577066-x64.psf">
		<parent buildCompare="EQ" serviceCompare="EQ" integrate="separate" disposition="detect">
			<assemblyIdentity name="Microsoft-Windows-Client-Features-Package-AutoMerged-inetcore" language="el-GR" version="6.3.9600.16384" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" buildType="release"/>
			<assemblyIdentity name="Microsoft-Windows-ServerCore-SKU-Foundation-Package-inetcore" language="el-GR" version="6.3.9600.16384" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" buildType="release"/>
			<assemblyIdentity name="Microsoft-Windows-Server-Gui-Shell-Package-inetcore" language="el-GR" version="6.3.9600.16384" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" buildType="release"/>
		</parent>
		<installerAssembly name="Microsoft-Windows-ServicingStack" version="6.0.0.0" language="neutral" processorArchitecture="amd64" versionScope="nonSxS" publicKeyToken="31bf3856ad364e35"/>
		<update name="4577066-1603_neutral_GDR">
			<applicable disposition="staged">
				<updateComponent elevate="distribution">
					<assemblyIdentity name="Microsoft-Windows-IE-Plu