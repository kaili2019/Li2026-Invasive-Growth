% match parent invasive with surface growth dataset
% 
% Kai Li
% 25 Mar 2026

clear
clc

load("invasive_D3.mat");
load("surface_D3.mat");

%% EXPD_Day_3: match data to get outliers

cc = 1;
for i = 1:length(invasive_D3)
    tmp_name = invasive_D3{i,3}(1:end-1);
    if ~contains(tmp_name,surface_D3(:,3))
%         disp(tmp_name)
        rm_idx_D3W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_D3)
    tmp_name = surface_D3{i,3}+"w";
    if ~contains(tmp_name,invasive_D3(:,3))
%         disp(tmp_name)
        rm_idx_D3S(cc) = i;
        cc = cc+1;
    end
end

%% remove mismatched files

if length(invasive_D3) == 166
    invasive_D3(rm_idx_D3W,:) = [];
end

if length(surface_D3) == 179
    surface_D3(rm_idx_D3S,:) = [];
end

if length(invasive_D3) == 152
    invasive_D3(112,:) = [];
end

%%

load("invasive_D6.mat");
load("surface_D6.mat");

%% EXPD_Day_6: match data to get outliers
cc = 1;
for i = 1:length(invasive_D6)
    tmp_name = invasive_D6{i,3}(1:end-1);
    if ~contains(tmp_name,surface_D6(:,3))
%         disp(tmp_name)
        rm_idx_D6W(cc) = i;
        cc = cc+1;
    end
end

%%
cc = 1;
for i = 1:length(surface_D6)
    tmp_name = surface_D6{i,3}+"w";
    if ~contains(tmp_name,invasive_D6(:,3))
%         disp(tmp_name)
        rm_idx_D6S(cc) = i;
        cc = cc+1;
    end
end

%%
if length(invasive_D6) == 217
    invasive_D6(rm_idx_D6W,:) = [];
end

if length(surface_D6) == 195
    surface_D6(rm_idx_D6S,:) = [];
end

add_rm_D6W = [9,80,81,107,147,153,159,165,180];

if length(invasive_D6) == 199
    invasive_D6(add_rm_D6W,:) = [];
end
%% EXPD_Day_3: tabulate data

ND3 = length(invasive_D3);

tab_D3 = cell(ND3,11);

for i = 1:ND3

    % 1. full name or ID
    if contains(invasive_D3{i,3},'796')
        if invasive_D3{i,3}(5:6) == "BD"
            tab_D3{i,1} = "parent";
        else
            tab_D3{i,1} = invasive_D3{i,3}(5:8);
        end
    else
        tab_D3{i,1} = invasive_D3{i,3}(1:5);
    end
    
    % 2. suphide (Y/N)
    if contains(invasive_D3{i,3},'400S')
        tab_D3{i,2} = 'Y';
    else
        tab_D3{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_D3(:,3),invasive_D3{i,3}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_D3{tmp_idx,1};
    tab_D3{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_D3{i,1};
    tab_D3{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_D3{i,5} = tab_D3{i,4}/tab_D3{i,3};
    
    % 6. Time of trial 
    tab_D3{i,6} = "Nov_18";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_D3{i,3},'BD50')
        tab_D3{i,7} = "BD50";
    elseif contains(invasive_D3{i,3},'BD75')
        tab_D3{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    tab_D3{i,8} = nan;
    
    % 9. Number of days: 3,4 or 6
    tab_D3{i,9} = "day3";
    
    % 10. Experiment type: A,B,C,D or E
    tab_D3{i,10} = "ExpD";

    % 11. Full name
    tab_D3{i,11} = invasive_D3{i,3}(1:end-1);
end

%% EXPD_Day_3: concantenate to a single table 

Id = 1:length(tab_D3);

df_D3 = table;

df_D3.yeast_id = Id';
df_D3.mutant_id = tab_D3(:,1);
df_D3.sulfide = tab_D3(:,2);
df_D3.surface_area = tab_D3(:,3);
df_D3.washed_area = tab_D3(:,4);
df_D3.area_ratio = tab_D3(:,5);
df_D3.time_of_exp = tab_D3(:,6);
df_D3.nutrient = tab_D3(:,7);
df_D3.slad = tab_D3(:,8);
df_D3.day = tab_D3(:,9);
df_D3.exp_type = tab_D3(:,10);
df_D3.full_name = tab_D3(:,11);

%% EXPD_Day_3: write table to csv

% writetable(df_D3,'slad_expD_day3.csv')

%% EXPD_Day_6: tabulate data

ND6 = length(invasive_D6);

tab_D6 = cell(ND6,11);

for i = 1:ND6

    % 1. full name or ID
    if contains(invasive_D6{i,3},'796')
        if invasive_D6{i,3}(5:6) == "BD"
            tab_D6{i,1} = "parent";
        else
            tab_D6{i,1} = invasive_D6{i,3}(5:8);
        end
    else
        tab_D6{i,1} = invasive_D6{i,3}(1:5);
    end

    % 2. suphide (Y/N)
    if contains(invasive_D6{i,3},'400S')
        tab_D6{i,2} = 'Y';
    else
        tab_D6{i,2} = 'N';
    end
    
    tmp_idx = find(contains(surface_D6(:,3),invasive_D6{i,3}(1:end-1)));
    
    % 3. surface area 
    Itmp = surface_D6{tmp_idx,1};
    tab_D6{i,3} = sum(sum(Itmp<0.5));
    
    % 4. washed area
    Jtmp = invasive_D6{i,1};
    tab_D6{i,4} = sum(sum(Jtmp));
    
    % 5. Area ratio
    tab_D6{i,5} = tab_D6{i,4}/tab_D6{i,3};
    
    % 6. Time of trial 
    tab_D6{i,6} = "Nov_21";
    
    % 7. nutrient: BD50 or BD75
    if contains(invasive_D6{i,3},'BD50')
        tab_D6{i,7} = "BD50";
    elseif contains(invasive_D6{i,3},'BD75')
        tab_D6{i,7} = "BD75";
    end
    
    % 8. SLAD: 1x or 2x 
    tab_D6{i,8} = nan;
    
    % 9. Number of days: 3,4 or 6
    tab_D6{i,9} = "day6";
    
    % 10. Experiment type: A,B,C,D or E
    tab_D6{i,10} = "ExpD";

    % 11. Full name
    tab_D6{i,11} = invasive_D6{i,3}(1:end-1);
end

%% EXPD_Day_6: concantenate to a single table 

Id = 1:length(tab_D6);

df_D6 = table;

df_D6.yeast_id = Id';
df_D6.mutant_id = tab_D6(:,1);
df_D6.sulfide = tab_D6(:,2);
df_D6.surface_area = tab_D6(:,3);
df_D6.washed_area = tab_D6(:,4);
df_D6.area_ratio = tab_D6(:,5);
df_D6.time_of_exp = tab_D6(:,6);
df_D6.nutrient = tab_D6(:,7);
df_D6.slad = tab_D6(:,8);
df_D6.day = tab_D6(:,9);
df_D6.exp_type = tab_D6(:,10);
df_D6.full_name = tab_D6(:,11);

%% EXPD_Day_6: write table to csv

% writetable(df_D6,'slad_expD_day6.csv')
