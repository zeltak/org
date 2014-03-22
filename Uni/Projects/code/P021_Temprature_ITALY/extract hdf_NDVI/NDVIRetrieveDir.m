function DirectoryToProcess = NDVIRetrieveDir ()

    clear DirectoryToProcess
    DirectoryToProcess=struct ('Name',[], 'Parent',[], 'FullPath',[]); 
    %Obtain the directories list

    sub = dir(chdir);
    %Create the logic vector
    isub = [sub(:).isdir]; 
    %Retrieve the directory
    SubDirList = {sub(isub).name};
    %Delete . and ..
    SubDirList(ismember(SubDirList,{'.','..'})) = [];
    
    %If no directory was found exit
    if (isempty(SubDirList))
        DirectoryToProcess = [];
        return;
    end
    
    %Create the structure
    DirOrder = 1;
    for i =1:1:length(SubDirList)
        DirectoryToProcess(DirOrder).Name = char(SubDirList(i));
        DirectoryToProcess(DirOrder).Parent = char(chdir);
        if (isunix)
            DirectoryToProcess(DirOrder).FullPath = [char(chdir) '/' char(SubDirList(i))];
        else
            DirectoryToProcess(DirOrder).FullPath = [char(chdir) '\' char(SubDirList(i))];
        end
        DirOrder = DirOrder+1;
    end

end
