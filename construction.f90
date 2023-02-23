
!======
!eg: 三个气象站降水的的例子

program Two_Chap_rain
implicit none
real r11 ,r12, r13 , r14 , r15 , total11 , av11
real r21 ,r22, r23 , r24 , r25 , total21 , av21
real r31 ,r32, r33 , r34 , r35 , total31 , av31
real av1, av2, av3, av4, av5

write(*,"(28X , '5月 6月 7月 8月 9月')")
write(*,"(1X, '输入江阴气象站5个月的降雨量:' , /)")
read(*,*) r11 ,r12, r13 , r14 , r15 , total11 , av11

write(*,"(1X, '输入定波闸气象站5个月的降雨量',/)")
read(*,*) r21 ,r22, r23 , r24 , r25 , total21 , av21

write(*,"(1X, '输入肖山气象站5个月的降雨量', /)")
read(*,*) r31, r32, r33, r34,r35, total31, av31

! 江阴总降雨量和平均值
total11 = r11+r12+r13+r14+r15
av1 = total11/5

! 定波闸总降雨量和平均值
total21 = r21+r22+r23+r24+r25
av2 = total21/5

! 肖山总降雨量和平均值
total31 = r31+r32+r33+r34+r35
av3 = total31/5

! 三个气象站总降雨量在5月的平均值
av1 = (r11+r21+r31)/3

! 三个气象站总降雨量在6月的平均值
av2 = (r12+r22+r32)/3

! 三个气象站总降雨量在7月的平均值
av3 = (r13+ r23 + r33)/3

! 三个气象站总降雨量在8月的平均值
av4 = (r14+ r24 + r34)/3

! 三个气象站总降雨量在9月的平均值
av5 = (r15 + r25 + r35)/3


! 设置格式

write(*,"(26X, '5月 6月 7月 8月 9月 总雨量 平均值')")
write(*, 200)'江阴气象站5个月的降雨量:' , r11, r12, r13, r14, r15, total11, av11
write(*,200)'定波闸气象站5个月的降雨量:' , r21, r22, r23, r24, r25, total21, av21
write(*,200)'肖山气象站5个月的降雨量:' , r31, r32, r33, r34, r35, total31, av31
200 format(1X, A22 , 5(F5.1, 2X), F6.1, 2X, F7.3)
! 设置格式300
write(*, 300)'5月', av1, '6月', av2, '7月', av3 , '8月',av4, '9月', av5
300 format(1X, A4, '平均雨量:' , F7.3)

end



!=========================例子
!eg1:
!从键盘输入一个气温值, 如果大于35.0,则显示在屏幕上。
program MyTest2
implicit none
real T 
    read*,T
    if(T>35.0) then
    print* , '这是高温'
    print*, 'T=', T
    end if
end

! 生成exe 文件后没啥显示的, 直接输入数字就行, 比如100  
! 就可以看到 输入的结构了



eg2:
由于大气受到污染，一些地区开始形成酸雨区，酸雨是指PH值小于5.6的雨雪或其他形式的大气降水
通过收集水样测量其PH值，判断它的酸碱性并打印出来。根据题意设计算法并画出程序流程图，如图所示^^^^

program test3
implicit none
real ph
write (*,*)'Please enter PH value:'
read *, ph
if(ph<5.6) then
    write(*,100)ph
else
    write(*,200)ph
end if

100 format(1X, 'PH=', F4.2,'is acid rain!')
200 format(1X, 'PH=',F4.2,'is not acid rain')
end



! eg3
! 这个是题目
https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230222201725287.png

program test4
implicit none
    real r
    write(*,100)
    read*, r
    if (r<5) then
        print 200
    else if(r<15) then
        print 300
    else if(r<30) then
        print 400
    else if(r<70) then
        print 600
    else if(r<140) then
        print 600
    else
        print 700
    end if

    100 format(1X,'请输入12小时降雨量')
    200 format(1X, '小雨')
    300 format(1X, '中雨')
    400 format(1X,'暴雨')
    600 format(1X, '大暴雨')
    700 format(1X, '特大暴雨')

    end


! 问题, 编译完成后没法看清结果

!eg4:
! 下面是题目的截图
!https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230222210845946.png

program test_atmos_stable
implicit none

real N2 
write(*,100)
read(*,*)N2
if(N2.EQ.0.0)print*, '中性层结'
if(N2.GT. 0.0)print*, '稳定'
if(N2.LT. 0.0)pirnt*, '不稳定'

100 format(1X, 'please input N2 :')


end



!=============
!eg5:
! https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230222221945815.png
! 对所有的风速进行输出
program test5
implicit none

real wind_velocity
print*,'请输出风速大小'
read*, wind_velocity
select case (int(wind_velocity))
    case(0:2)
        print*, '0级'
        print* ,'无风'

    case(3:5)
    print*, '1级'
        print*, '软风'

! ^^^剩下的都一样, 这里就不写了
    case default
    print*, '非法数据'
end select 




end



!===========
!eg6:  多个if 嵌套的例子

program moreIF
implicit none
    read*, a,b,cased = b**2-4.0*a*c
    if (a==0.0) then
        if(b==0.0) then 
            if(c==0.0) then
         print*,'平凡解'
         else
         pirnt*,'无解'
         end if

       end if

        else
        print*, '一个实根'
        print*, -c/d

        endif
    
    else
    end if

end




!===========
!eg   风速的问题
! 这是问题存放的地址:   https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223083104978.png
program uv
implicit none
real u, v
read*, u,v
if (u>0.0) then
if (v>0.0) then
    print*, '西南风'
else if (v<0.0) then
    print*, '西北风'
else 
    print*, '西风'

end if

else if (u<0.0) then
    if (v>0.0) then
    print*, '东南风'

    else if(v<0.0) then
        print*, '东北风'

    else 
        print *, '东风'
end if

else
    if (v>0.0) then
        print*,'南风'
    else if (v<0.0) then
        print*,'北风'
    else
        print*, '无风'
end if

end if

end

! =====================do 循环
!eg 

do i= 1,3,2
    m = i*i
    print*,i, m
end do
print*, i,'因为 此时的i 超过了3, 所以i=5 是不会开始循环的, ',/,'所以正常执行完一个循环后, i = n+1'
end


! ===========eg   : exit 循环
! 题目: https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223085404943.png
program test_exit
implicit none

real ::s=0 , t
integer ::i, n
read *, n
do  i= 1,n    ! 不写步长, 默认步长为 1
    t = 1./(i*(i+1))
    s = s+t
    if(abs(t)<= 1.E-5) exit

end do
    if (i== n+1) i=i-1
    print*, 'sum=',s,'term=',i      ! 注意: 因为是中途终端了循环, 所此处的i 仍然为 i,  不用 +1

end


! ===========cycle 循环
! eg: 顺序输出1-10序列中除了9以外的其它数字

do i= 1,10
    if (i==9)  cycle  ! 跳过本次循环, 接着下一个循环.
    print*, i
enddo
end

! eg 统计降雨量
real precip
integer k
print*, '输入每6小时的降雨
k=0

read * , precip 
do  while(precip.ge.0.0 and precip .le. 1000)
    k = k+1
    print*,k, '降水量' , precip
    read *, precip
end do
print *, '输入数据异常'
end




! =====================循环
! eg: 将字符输出出来
!地址:  https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223094903475.png

program ym
implicit none
    integer iy, im
    character year*4 , mo*2     ! 此处设置 year 为4个字符, mo 为2个字符
    character dir*100          ! 此处 dir 设置为100个字符长度, 足够长了.

    do iy= 1979, 1990
        write(year(1:4), '(i4)')iy   ! 此处取iy 的四个数字, 赋值给year, 这样就不build将iy 的值直接打在屏幕上了.
        mo= ' '
 
    do im = 1, 12
        if(im<10) then
            write(mo(1:1), '(i1)')im
        else
            write(mo(1:2),'(i2)')im
        endif

    dir = 'e:\data\'// trim(year) // '\' // trim(mo) // '\precip'
    print*, trim(dir)

! trim 作用是出去字符串中的空格. 


    end do
    end do

! 注意: 循环非常注意包裹的顺序, 如果包裹错了, 那就很大可能出现意外了.

end


!======= 计算降水缺测值=========
! 注意该题尚未听懂, 还需在看一下
program global_precip
implicit none

integer, parameter ::nt= 115 , nx=360 , ny=180
! nt 为时次总数    , nx 代表 纬向的格点 , ny 代表径向格点

real , parameter  ::undef = -9.99E+08   ! undef 代表却测值
real ::pr(nx, ny, nt) , ap(nx,ny,nt)
real ::r(nt), ar(nt)
real ::ave
integer ::i, j, k 

!   =====读取数据===============
open(1,file='E:\Console1\rain.sum.gpcc.grd',form='binary')

do k=1,nt   ! 时次
    do j=1,ny! 径向格点
        do i=1,ny
            read(1)pr(i,j,k)   !
        end do
    end do
end do

close(1)

ap = undef
do j=1,ny
    do i=1,nx
        do k=1,nt
            r(k) = pr(i,j,k)
        end do
        
    end do

    if (r(1).eq.undef)cycle     !若缺测, 表示海洋不计算水平距
    call anomaly(nt,r,ave, ar) ! 如果不是缺测值, 计算水平
    do k = 1,nt
        ap(i,j,k) = ar(k)
    end do
    
end do

! =========写数据=========
open(unit=2, file='E:/console1/rain.anomaly.grd', form='binary')
do k=1,nt   ! 时次
    do j=1,ny
        do i= 1,nx
            write(2)ap(i,j,k)
        end do
    end do

end do
close(2)
write(*,*)'write, OK'

end


subroutine anomaly(n,x, ave,ax)
implicit none 

integer :: n,i
real ::ave, sigma, x(n), ax(n)
sigma = 0.0
do i = 1,n
    sigma = sigma + x(i)
end do

ave = sigma/real(n)
do i = 1,n
    ax(i) = x(i) -ave      ! 计算水平距


end  subroutine anomaly


! =======一维数组的输入和输出
program implicit_do
integer a(5)
read (*,100)a
100 format(5I3)

! 输出a(1) , a(3), a(5)
write(*,200)(a(i), i=1,5,2)

200 format(1X,3I3)

end

! 输入 1,2,3,4,5  , 输出: 1,3,5




!==========找最大数===========
!https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223175447697.png

program maxNum
implicit none

integer i, n, max
real, allocatable ::a(:)  ! 定义a 为动态数组
print*, 'Enter n:'
read*, n
allocate(a(n))             ! 分配内存空间
print*,'输入',n,'个温度'

read*,(a(i), i=1,n)
max = 1
do i=2,n
if(a(i)>a(max)) max = i
enddo
print*,'max=',a(max),'在第', max, '个时次'

deallocate(a)              ! 释放存储空间

end


! ====eg===========这是气象要素平均值
! https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223180903916.png

! 此题, m,n 是通过键盘输入得到的, 之后又用到了i ,j , 这两个的值是m,n 赋值给的,  也就是m,n
是为了 方便i, j 在之后do 循环中次数的表示吗?   是否可以用while来表示呢, 这样就不用i, j 了,
但是 while 循环后, 变量可以在循环体的外部使用并且表示吗?

program all_item
implicit none
real total,mave
integer i,j,m,n
real,allocatable ::matrix(:,:)
print*, 'Enter m&n'
read* , m,n
allocate(matrix(m,n))
print*,'输入',m,'*',n,'矩阵数据:'    ! 此处输入 m, n  的值, 有m,n  来确定数组的形状到是几行几列.      
read* ,((matrix(i,j),j=1,n),i=1,m)    ! ((matrix(i,j)j=1,n),i=1,m)    ! 这里再次输入动态数组, i 就是m ,j 就是n
    total = 0.0
do i=1,m
    do j=1,n
        total = total+ matrix(i,j)
    end do

end do
mave = total/(m*n)
print* , 'mave=',mave
deallocate(matrix)
end

! =======================eg
! ==========数组常用算法举例
! 问题: https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223195550803.png

program common_case
implicit none

! =======定义变量=================
integer,parameter ::N = 31
character*6 num(N)
integer i
real s(N),sum, aver

!=========main===============

write (*,*)'输入日期和气温:'
read(*,*)(num(i),s(i),i=1,N)
sum = 0.0 

do  i= 1,n
    sum = sum + s(i)
end do

aver = sum/N 
write(*,*)'平均值是:',aver

write(*,*)'日期      气温'
write(*,*)'--------------'
do i = 1,N 
    if ( s(i) >= aver )   write(*,200)num(i),s(i)

end do 
write(*,*)'--------------'
200 format(A8, F6.1)



! eg===============
! 问题地址: https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223195649957.png


program LonLot
implicit none

!----------param--------------
integer i,j, h(3,3), hmin,imin, jmin
real lon(3,3),lat(3,3)
data h/5350,5300,5300,5300,5300,5250,5350,5300,5400/
write(*,'(1X,3I5)')((h(i,j),i=1,3),j=3,1,-1)          ! j 是到这取的

! --------main-----------------------
do j=1,3
    do i=1,3
        lon(i,j)=135.0 + (i-1)*5
    end do
end do

write(*,'(1X3F6.1)')((lon(i,j),i=1,3),j=3,1,-1)
do j= 1,3
    do i=1,3
        lat(i,j) = 50.0 + (j-1)*5
    end do
end do

write(*,'(1X,3F6.1)')((lat(i,j),i=1,3),j=3,1,-1)
hmin = 9000 
do j=1,3
    do i=1,3
        if (h(i,j)<hmin) then
        hmin = h(i,j)
        imin = i
        jmin = j
        end if
    end do
end do

write(*,*)'hmin=',hmin,'long=',lon(imin,jmin),'lat=',lat(imin,jmin)

end



!==========================
!----------------------eg 数组应用最后一道
! 问题地址: https://cdn.jsdelivr.net/gh/wangyuhubugui/BJYH_picture@main/img/image-20230223210946956.png

program FourProblem
implicit none

! --------param--------------
integer,parameter ::m=5,n=4    
real t(m,n) ,DAT(m), TAT(n)   ! t 表示存放的所有温度,   DAT表示存放每天的平均温度, TAT表示存放每个时刻的平均温度
real sum,aver,RS, CS, MaxT    ! MaxT 表示每天平均温度最高值 , RS 每行元素的和, CS表示每列元素的和, sum 表示所有元素的和
integer i,j

data t/28.8,29.8, 28.7,29.9,30.4,32.9,31.8,32.3, 33.4,32.5 ,&
36.8,36.0,35.1,36.2,36.5,33.2,31.1,32.3,32.7,25.5/

! --------------main----------------
!  统计每天的平均温度和总的平均温度
sum = 0.0
do i=1,m
    RS=0
    do j=1,n
        RS = RS+t(i,j)
        sum = sum+t(i,j)
    end do
    DAT(i) = RS/n
end do

aver = sum/(m*n)

! ------------------------
!  统计没5天中每天4个观测时次的温度平均值
do j= 1,n
    CS=0
    do i = 1,m
    CS = CS+t(i,j)
    end do

    TAT(j)  = RS/m

end do

! 输出温度数据及统计结果
write(*,*)"每天的平均温度",DAT
write(*,*)"5天总的平均温度" , aver
write(*,*) "每天4个观测时次的平均温度", TAT

! 求每天平均温度的最高温度
MaxT = DAT(1)
do i =2,m
    if ( DAT(i) >MaxT )MaxT=DAT(i)
end do


write(*,*)"每天平均温度最高值",MaxT


end
