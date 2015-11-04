
function [resp, responseKeyHit] = getObserverInputFS(maxTime)

% check if a key has been pressed
[keyIsDown, ~, keyCode] = KbCheck;
% wait for keypress
t0 = GetSecs;
while ~keyIsDown
     [keyIsDown, ~, keyCode] = KbCheck;
     % check for time out
     if (GetSecs-t0) > maxTime
         resp = NaN;
         responseKeyHit = 1;
         break;
     end
end
% check what key was pressed
if find(keyCode) == KbName('f');
     responseKeyHit = 1;
     resp = 1;
elseif find(keyCode) == KbName('j');
     responseKeyHit = 1;
     resp = -1;
elseif  find(keyCode) == KbName('q');
     responseKeyHit = 1;
     resp = 3;
else
     resp = 3;     
end
end
