function pilotExperiment


% visual degrees 1 pixel = 0.0309

close all
% %% get subject number and randomise random seed
subjNum = input('input subject number: ');
RandStream('mt19937ar', 'Seed', subjNum);
% % RandStream.setDefaultStream(stream);


params.bkgrndColour = 125;
params.delta = 256;
params.N = 48;
params.r1 = 20;
params.r2 = 12;
params.w = 6;
params.runLengths = [3 4 5];
params.targetColour = [.3, 0.9, .6];
params.R = 256; % radius of ring which items are drawn on

params.targetPreviewDisplayTime = 0.1;
params.gapTime = 1;
params.maxStimDisplayTime = [2, 4];

blocks = {'coloured', 'uncoloured'};

nRunsPerCond = 5;

%% create balanced list of visualise/practise runs
ii = 0;
for run=1:nRunsPerCond
    ii=ii+1;
    conditionList(ii).visualise = 'no';
    ii=ii+1;
    conditionList(ii).visualise = 'yes';
end
% randomise order
conditionList = conditionList(randperm(ii));
clear ii

usePreMadeCs = 1;

%% make landolt Cs at eigth orientations
if usePreMadeCs
   load(['stimuli/c_']);
else
    ii = 0;
    for phi = 0:45:315
        ii = ii+1;
        
        landoltC{ii}  = drawLandoltC(params.N, params.r1, params.r2, params.w, phi);
    end
end

%% set up screen
stimuliScrn = Screen('OpenWindow', 0, params.bkgrndColour, [0 0 1024 1024]);

Screen('TextFont', stimuliScrn, 'Helvetica');
Screen('TextSize', stimuliScrn, 30);
Screen('TextColor', stimuliScrn, [255 255 255])

%% make fixation cross
fixcross = zeros(1024);
fixcross = drawFixcross(fixcross);
fixcross = repmat(fixcross, [1 1 3]);
t_fixcross =  Screen('MakeTexture', stimuliScrn, 255*fixcross);

blank = zeros(1024);
t_blank = Screen('MakeTexture', stimuliScrn, blank);
clear blank

% set up results file
fresults = fopen(['results/' int2str(subjNum) 'results.txt'], 'w');
fprintf(fresults, 'person, block, runNumber, visualise, trialNumber, targetPresent, responseTime, response\n');

% set to some intial value
previousTargOri = -1;

%% start block
for blk = 1:2
    
    block = blocks{blk};
    
    if strcmp(block, 'coloured')
        targetColour = params.targetColour;
    else
        targetColour = [1 1 1];
    end
    
    Screen('DrawTexture', stimuliScrn, t_blank);
    DrawFormattedText(stimuliScrn, 'Press a button to continue', 'center', 384);
    Screen(stimuliScrn, 'flip');
    WaitSecs(1);
    KbWait();
    
    for run = 1:length(conditionList)
        % pick a random run length from those given in params.runLengths
        rl = params.runLengths(randi(length(params.runLengths)));
        
        % get target orientation, and list of distracter oris
        % randomly decide target orientation
        % make sure it isn't same as previous run
        targOri = -1;
        while targOri == previousTargOri
            targOri = randi(8);
        end
        
        % for each trial in this run
        for tr = 1:rl
            % decide if this is a target present of absent trial
            targPresent = rand<0.5;
            
            % get distracter orientations
            [distracterOris, extraOri] = getTargDistOris(8, targOri);
            allOris = [targOri, distracterOris];
            allOris = allOris(randperm(12));
            
            % draw Cs
            stimuli = drawCstim;
            imwrite(stimuli, 'exStim.png')
            % draw preview
            preview = zeros(1024, 1024, 3);
            x1 = 512 - params.N/2 + 1;
            x2 = 512 + params.N/2;
            y1 = 512 - params.N/2 + 1;
            y2 = 512 + params.N/2;
            preview(x1:x2, y1:y2, 1) = targetColour(1) * landoltC{targOri};
            preview(x1:x2, y1:y2, 2) = targetColour(2) * landoltC{targOri};
            preview(x1:x2, y1:y2, 3) = targetColour(3) * landoltC{targOri};
            
            t_preview =  Screen('MakeTexture', stimuliScrn, 255*preview);
            t_stimuli =  Screen('MakeTexture', stimuliScrn, 255*stimuli);
            
            Screen('DrawTexture', stimuliScrn, t_fixcross);
            if strcmp(conditionList(run).visualise, 'yes') && (tr < 3)
                DrawFormattedText(stimuliScrn, 'Visualise Search', 'center', 384);
            end
            % display!
            Screen(stimuliScrn, 'flip');
            % wait for some random time
            fixcrossWaitTime = 1.2 + 0.4*rand;
            WaitSecs(fixcrossWaitTime);
            
            % display target preview
            Screen('DrawTexture', stimuliScrn, t_preview);
            if strcmp(conditionList(run).visualise, 'yes') && (tr < 3)
                DrawFormattedText(stimuliScrn, 'Visualise Search', 'center', 384);
            end
            % display!
            Screen(stimuliScrn, 'flip');
            WaitSecs(params.targetPreviewDisplayTime);
            
            % display a blank screen
            Screen('DrawTexture', stimuliScrn, t_blank);
            Screen(stimuliScrn, 'flip');
            WaitSecs(params.gapTime);
            
            % display stimulus
            Screen('DrawTexture', stimuliScrn, t_stimuli);
            Screen(stimuliScrn, 'flip');
            imageArray=Screen('GetImage', t_stimuli);
            imwrite(imageArray, 'exStimulus.png');
            % wait for key press or max time
            if strcmp(conditionList(run).visualise, 'no') || (tr > 2)
                tic;
                responseKeyHit = 0;
                while responseKeyHit == 0
                    [resp, responseKeyHit] = getObserverInputFS(params.maxStimDisplayTime(blk));
                    responseKeyHit=1;
                  
                end
                timeTakenForResp = toc;
            else
                WaitSecs(params.maxStimDisplayTime(blk));
                timeTakenForResp = NaN;
                resp = NaN;
            end
            
            % display inter-trial interval
            Screen('DrawTexture', stimuliScrn, t_blank);
            Screen(stimuliScrn, 'flip');
            intertrialWaitTime = 1.2 + 0.4*rand;
            WaitSecs(intertrialWaitTime);
            
            clear intertrialWaitTime fixcrossWaitTime
            
            % output results
            % person, block, runNumber, visualise, trialNumber, targetPresent, resposneTime, response
            fprintf(fresults, '%d, %s, %d, %s, %d, %d, %f, %d\n', ...
                subjNum, block,  run, conditionList(run).visualise, tr, targPresent, timeTakenForResp, resp);
            clear timeTakenForResp resp
        end
    end
end
sca

    function stimuli = drawCstim
        % start fix fixation cross
        stimuli = fixcross;
        
        if strcmp(conditionList(run).visualise, 'no') || (tr > 2)
            % now draw the 12 Cs
            for p = 1:12
                x = round(params.R * cosd(p*360/12)) + 512;
                y = round(params.R * sind(p*360/12)) + 512;
                
                x1 = x - params.N/2 + 1;
                x2 = x + params.N/2;
                y1 = y - params.N/2 + 1;
                y2 = y + params.N/2;
                if allOris(p) == targOri
                    if targPresent == 1
                        tOri = targOri;
                    else
                        tOri = extraOri;
                    end
                    stimuli(x1:x2, y1:y2, 1) = targetColour(1) * landoltC{tOri};
                    stimuli(x1:x2, y1:y2, 2) = targetColour(2) * landoltC{tOri};
                    stimuli(x1:x2, y1:y2, 3) = targetColour(3) * landoltC{tOri};
                    clear tOri
                else
                    stimuli(x1:x2, y1:y2, :) = repmat(landoltC{allOris(p)}, [1 1 3]);
                end
                clear x1 x2 y1 y2 x y
            end
        end
    end

end

function im = drawFixcross(im)
im((512-31):(512+32), 511:512, :) = 0.8;
im(511:512, (512-31):(512+32), :) = 0.8;
end

function [distracterOris, extraOri] = getTargDistOris(nPhi, targOri)
% sort out distracter orientations
distracterOris = randperm(nPhi);
distracterOris(distracterOris==targOri) = [];
distracterOris = [distracterOris, distracterOris(1:4)];
extraOri = distracterOris(5);
distracterOris = distracterOris(randperm(11));
end

function c = drawLandoltC(n, r1, r2, w, phi)
% n = dimension of output element
% r1 = outer radius
% r2 = inner radius
% w = width of cut-out
% phi = angle of cut-out
x = repmat((-n/2+1):(n/2), [n,1]);
d = x.^2 + x'.^2;
c = (d<r1^2) .* (d>r2^2);
c(1:(n/2), (n/2-w/2):(n/2+w/2)) = 0;
c = imrotate(c, phi, 'nearest', 'crop');
c = imfilter(c, fspecial('gaussian', 5, 1));
end