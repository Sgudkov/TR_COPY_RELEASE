# Copy or copy and release requests

### This example allow you to create transport of copies automatically

#### Sometimes need to move development to another system without workbench TR release. Here is useful tool to do it automatically instead of do it yourself.


>Source code [here](https://github.com/Sgudkov/TR_COPY_RELEASE/blob/main/TR_COPY.abap).

#### 1. Simple selection screen to choose transport request.

![alt text](https://github.com/Sgudkov/TR_COPY_RELEASE/blob/main/SEL_SRC.jpg).

#### 2. Here how grid looks like.

![alt text](https://github.com/Sgudkov/TR_COPY_RELEASE/blob/main/GRID.jpg).

> Your can overview only your requests.

#### 3. Buttons "Copy and release" and "Copy".

- [x] "Copy" 
> Allow you to copy selected transport request with all including objects and included tasks.
> Notice that if task doesn't contain objects with type 'R3TR' this task will not copy.
> Description of new "Copy" obtain from copied request.

- [x] "Copy and release" 
> After create copy TR, release it without any check.