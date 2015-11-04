function preRenderCs


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

ii = 0;
for phi = 0:45:315
    ii = ii + 1;
    landoltC{ii}  = drawLandoltC(params.N, params.r1, params.r2, params.w, phi);
end
save(['stimuli/c_.mat'], 'landoltC');
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